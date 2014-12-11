// This object will hold all exports.
var Haste = {};

/* Thunk
   Creates a thunk representing the given closure.
   Since we want automatic memoization of as many expressions as possible, we
   use a JS object as a sort of tagged pointer, where the member x denotes the
   object actually pointed to. If a "pointer" points to a thunk, it has a
   member 't' which is set to true; if it points to a value, be it a function,
   a value of an algebraic type of a primitive value, it has no member 't'.
*/

function T(f) {
    this.f = f;
}

function F(f) {
    this.f = f;
}

// Special object used for blackholing.
var __blackhole = {};

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    if(f instanceof T) {
        f = E(f);
    }
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!(f instanceof Function)) {
        f = B(f);
        if(!(f instanceof Function)) {
            return f;
        }
    }

    if(f.arity === undefined) {
        f.arity = f.length;
    }
    if(args.length === f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return f(args[0]);
            default: return f.apply(null, args);
        }
    } else if(args.length > f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return A(f(args.shift()), args);
            default: return A(f.apply(null, args.splice(0, f.arity)), args);
        }
    } else {
        var g = function() {
            return A(f, args.concat(Array.prototype.slice.call(arguments)));
        };
        g.arity = f.arity - args.length;
        return g;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f != __blackhole) {
            var f = t.f;
            t.f = __blackhole;
            t.x = f();
        }
        return t.x;
    } else {
        return t;
    }
}

/* Bounce
   Bounce on a trampoline for as long as we get a function back.
*/
function B(f) {
    while(f instanceof F) {
        var fun = f.f;
        f = __blackhole;
        f = fun();
    }
    return f;
}

// Export Haste, A, B and E. Haste because we need to preserve exports, A, B
// and E because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;
window['B'] = B;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw err;
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [0, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
if(Math.imul) {
    var imul = Math.imul;
} else {
    var imul = function(a, b) {
        // ignore high a * high a as the result will always be truncated
        var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
        var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
        var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
        return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
    }
}

function addC(a, b) {
    var x = a+b;
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [0, sign*man, exp];
}

function decodeDouble(x) {
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return [0, sign, manHigh, manLow, exp];
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = B(A(f, [[0, str.charCodeAt(i)], acc]));
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,[0,str.charCodeAt(i)],new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1])[1]);
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = B(A(f, [mv.x]));
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['v']['i8'][0] = 'U'.charCodeAt(0);
    le['v']['i8'][1] = 'T'.charCodeAt(0);
    le['v']['i8'][2] = 'F'.charCodeAt(0);
    le['v']['i8'][3] = '-'.charCodeAt(0);
    le['v']['i8'][4] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return [0];
    } else if(a == b) {
        return [1];
    }
    return [2];
}

function jsCatch(act, handler) {
    try {
        return B(A(act,[0]));
    } catch(e) {
        return B(A(handler,[e, 0]));
    }
}

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round; // Stupid GHC doesn't like periods in FFI IDs...
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
}

function jsLog(val) {
    console.log(val);
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return val == undefined ? '' : val.toString();
}

function jsEval(str) {
    var x = eval(str);
    return x == undefined ? '' : x.toString();
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.currentTarget.offsetLeft || 0),
	    posy - (e.currentTarget.offsetTop || 0)];
}

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                B(A(cb,[[0,k.keyCode],0]));
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,x.button],[0,mx,my],0]));
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,mx,my],0]));
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {B(A(cb,[[0,x.keyCode],0]));};
        break;        
    default:
        fun = function() {B(A(cb,[0]));};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return true;
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return true;
    }
    return false;
}

function jsSetTimeout(msecs, cb) {
    window.setTimeout(function() {B(A(cb,[0]));}, msecs);
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetAttr(elem, prop) {
    if(elem.hasAttribute(prop)) {
        return elem.getAttribute(prop).toString();
    } else {
        return "";
    }
}

function jsSetAttr(elem, prop, val) {
    elem.setAttribute(prop, val);
}

function jsGetStyle(elem, prop) {
    return elem.style[prop].toString();
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,[0,e]];
    }
    return [0];
}

function jsQuerySelectorAll(elem, query) {
  var els = [0],
      len, nl, i;

  if (!elem || typeof elem.querySelectorAll !== 'function') {
    return els;
  }

  nl = elem.querySelectorAll(query);
  len = nl.length;

  for (i=len-1; i >= 0; --i) {
    els = [1, [0, nl[i]], els];
  }

  return els;
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsCreateTextNode(str) {
    return document.createTextNode(str);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,[0,elem]];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetFirstChild(elem) {
    var len = elem.childNodes.length;
    for(var i = 0; i < len; i++) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, [0,elem.childNodes[i]], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
}

function jsAppendChild(child, container) {
    container.appendChild(child);
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

var jsJSONParse = JSON.parse;

// JSON stringify a string
function jsStringify(str) {
    return JSON.stringify(str);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, [0, jsRead(obj)]];
    case 'string':
        return [1, [0, obj]];
        break;
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
        break;
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst_json(obj, 0)];
        } else if (obj == null) {
            return [5];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = [1, [0, [0,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst_json(arr,elem+1);})]
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, arr[elem], new T(function() {return arr2lst(arr,elem+1);})]
}

function lst2arr(xs) {
    var arr = [];
    for(; xs[0]; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);

    if(method == "POST") {
        xhr.setRequestHeader("Content-type",
                             "application/x-www-form-urlencoded");
    }
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                B(A(cb,[[1,[0,xhr.responseText]],0]));
            } else {
                B(A(cb,[[0],0])); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;

function makeStableName(x) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return x.stableName;
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

var I_fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

var I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(result, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += I_getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var I_getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var I_getBitsUnsigned = function(self, index) {
  var val = I_getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (I_getBits(self, i) != I_getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var I_greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var I_greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var I_lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var I_lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = I_getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = I_getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = I_getBits(self, i) >>> 16;
    var a0 = I_getBits(self, i) & 0xFFFF;

    var b1 = I_getBits(other, i) >>> 16;
    var b0 = I_getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return I_fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (I_lessThan(self, Integer.TWO_PWR_24_) &&
      I_lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = I_getBits(self, i) >>> 16;
      var a0 = I_getBits(self, i) & 0xFFFF;

      var b1 = I_getBits(other, j) >>> 16;
      var b0 = I_getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(I_greaterThan(self, Integer.ZERO) != I_greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (I_greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || I_greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) & I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) | I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) ^ I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i - arr_delta) << bit_delta) |
               (I_getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i + arr_delta) >>> bit_delta) |
               (I_getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = I_fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    return I_fromBits([x.getLowBits(), x.getHighBits()]);
}

function I_toInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

function I_fromWord64(x) {
    return x;
}

function I_toWord64(x) {
    return I_rem(I_add(__w64_max, x), __w64_max);
}

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var Long = function(low, high) {
  this.low_ = low | 0;
  this.high_ = high | 0;
};

Long.IntCache_ = {};

Long.fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Long.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Long(value | 0, value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Long.IntCache_[value] = obj;
  }
  return obj;
};

Long.fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Long.ZERO;
  } else if (value <= -Long.TWO_PWR_63_DBL_) {
    return Long.MIN_VALUE;
  } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
    return Long.MAX_VALUE;
  } else if (value < 0) {
    return Long.fromNumber(-value).negate();
  } else {
    return new Long(
        (value % Long.TWO_PWR_32_DBL_) | 0,
        (value / Long.TWO_PWR_32_DBL_) | 0);
  }
};

Long.fromBits = function(lowBits, highBits) {
  return new Long(lowBits, highBits);
};

Long.TWO_PWR_16_DBL_ = 1 << 16;
Long.TWO_PWR_24_DBL_ = 1 << 24;
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;
Long.ZERO = Long.fromInt(0);
Long.ONE = Long.fromInt(1);
Long.NEG_ONE = Long.fromInt(-1);
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);

Long.prototype.toInt = function() {
  return this.low_;
};

Long.prototype.toNumber = function() {
  return this.high_ * Long.TWO_PWR_32_DBL_ +
         this.getLowBitsUnsigned();
};

Long.prototype.getHighBits = function() {
  return this.high_;
};

Long.prototype.getLowBits = function() {
  return this.low_;
};

Long.prototype.getLowBitsUnsigned = function() {
  return (this.low_ >= 0) ?
      this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};

Long.prototype.isZero = function() {
  return this.high_ == 0 && this.low_ == 0;
};

Long.prototype.isNegative = function() {
  return this.high_ < 0;
};

Long.prototype.isOdd = function() {
  return (this.low_ & 1) == 1;
};

Long.prototype.equals = function(other) {
  return (this.high_ == other.high_) && (this.low_ == other.low_);
};

Long.prototype.notEquals = function(other) {
  return (this.high_ != other.high_) || (this.low_ != other.low_);
};

Long.prototype.lessThan = function(other) {
  return this.compare(other) < 0;
};

Long.prototype.lessThanOrEqual = function(other) {
  return this.compare(other) <= 0;
};

Long.prototype.greaterThan = function(other) {
  return this.compare(other) > 0;
};

Long.prototype.greaterThanOrEqual = function(other) {
  return this.compare(other) >= 0;
};

Long.prototype.compare = function(other) {
  if (this.equals(other)) {
    return 0;
  }

  var thisNeg = this.isNegative();
  var otherNeg = other.isNegative();
  if (thisNeg && !otherNeg) {
    return -1;
  }
  if (!thisNeg && otherNeg) {
    return 1;
  }

  if (this.subtract(other).isNegative()) {
    return -1;
  } else {
    return 1;
  }
};

Long.prototype.negate = function() {
  if (this.equals(Long.MIN_VALUE)) {
    return Long.MIN_VALUE;
  } else {
    return this.not().add(Long.ONE);
  }
};

Long.prototype.add = function(other) {
  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 + b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 + b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.subtract = function(other) {
  return this.add(other.negate());
};

Long.prototype.multiply = function(other) {
  if (this.isZero()) {
    return Long.ZERO;
  } else if (other.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  } else if (other.equals(Long.MIN_VALUE)) {
    return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().multiply(other.negate());
    } else {
      return this.negate().multiply(other).negate();
    }
  } else if (other.isNegative()) {
    return this.multiply(other.negate()).negate();
  }

  if (this.lessThan(Long.TWO_PWR_24_) &&
      other.lessThan(Long.TWO_PWR_24_)) {
    return Long.fromNumber(this.toNumber() * other.toNumber());
  }

  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 * b00;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a00 * b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.div = function(other) {
  if (other.isZero()) {
    throw Error('division by zero');
  } else if (this.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    if (other.equals(Long.ONE) ||
        other.equals(Long.NEG_ONE)) {
      return Long.MIN_VALUE;
    } else if (other.equals(Long.MIN_VALUE)) {
      return Long.ONE;
    } else {
      var halfThis = this.shiftRight(1);
      var approx = halfThis.div(other).shiftLeft(1);
      if (approx.equals(Long.ZERO)) {
        return other.isNegative() ? Long.ONE : Long.NEG_ONE;
      } else {
        var rem = this.subtract(other.multiply(approx));
        var result = approx.add(rem.div(other));
        return result;
      }
    }
  } else if (other.equals(Long.MIN_VALUE)) {
    return Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().div(other.negate());
    } else {
      return this.negate().div(other).negate();
    }
  } else if (other.isNegative()) {
    return this.div(other.negate()).negate();
  }

  var res = Long.ZERO;
  var rem = this;
  while (rem.greaterThanOrEqual(other)) {
    var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    var approxRes = Long.fromNumber(approx);
    var approxRem = approxRes.multiply(other);
    while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
      approx -= delta;
      approxRes = Long.fromNumber(approx);
      approxRem = approxRes.multiply(other);
    }

    if (approxRes.isZero()) {
      approxRes = Long.ONE;
    }

    res = res.add(approxRes);
    rem = rem.subtract(approxRem);
  }
  return res;
};

Long.prototype.modulo = function(other) {
  return this.subtract(this.div(other).multiply(other));
};

Long.prototype.not = function() {
  return Long.fromBits(~this.low_, ~this.high_);
};

Long.prototype.and = function(other) {
  return Long.fromBits(this.low_ & other.low_,
                                 this.high_ & other.high_);
};

Long.prototype.or = function(other) {
  return Long.fromBits(this.low_ | other.low_,
                                 this.high_ | other.high_);
};

Long.prototype.xor = function(other) {
  return Long.fromBits(this.low_ ^ other.low_,
                                 this.high_ ^ other.high_);
};

Long.prototype.shiftLeft = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var low = this.low_;
    if (numBits < 32) {
      var high = this.high_;
      return Long.fromBits(
          low << numBits,
          (high << numBits) | (low >>> (32 - numBits)));
    } else {
      return Long.fromBits(0, low << (numBits - 32));
    }
  }
};

Long.prototype.shiftRight = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >> numBits);
    } else {
      return Long.fromBits(
          high >> (numBits - 32),
          high >= 0 ? 0 : -1);
    }
  }
};

Long.prototype.shiftRightUnsigned = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >>> numBits);
    } else if (numBits == 32) {
      return Long.fromBits(high, 0);
    } else {
      return Long.fromBits(high >>> (numBits - 32), 0);
    }
  }
};



// Int64
function hs_eqInt64(x, y) {return x.equals(y);}
function hs_neInt64(x, y) {return !x.equals(y);}
function hs_ltInt64(x, y) {return x.compare(y) < 0;}
function hs_leInt64(x, y) {return x.compare(y) <= 0;}
function hs_gtInt64(x, y) {return x.compare(y) > 0;}
function hs_geInt64(x, y) {return x.compare(y) >= 0;}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {return x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {return x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {return x.shiftRightUnsigned(bits);}
function hs_intToInt64(x) {return new Long(x, 0);}
function hs_int64ToInt(x) {return x.toInt();}



// Word64
function hs_wordToWord64(x) {
    return I_fromInt(x);
}
function hs_word64ToWord(x) {
    return I_toInt(x);
}
function hs_mkWord64(low, high) {
    return I_fromBits([low, high]);
}

var hs_and64 = I_and;
var hs_or64 = I_or;
var hs_xor64 = I_xor;
var __i64_all_ones = I_fromBits([0xffffffff, 0xffffffff]);
function hs_not64(x) {
    return I_xor(x, __i64_all_ones);
}
var hs_eqWord64 = I_equals;
var hs_neWord64 = I_notEquals;
var hs_ltWord64 = I_lessThan;
var hs_leWord64 = I_lessThanOrEqual;
var hs_gtWord64 = I_greaterThan;
var hs_geWord64 = I_greaterThanOrEqual;
var hs_quotWord64 = I_quot;
var hs_remWord64 = I_rem;
var __w64_max = I_fromBits([0,0,1]);
function hs_uncheckedShiftL64(x, bits) {
    return I_rem(I_shiftLeft(x, bits), __w64_max);
}
var hs_uncheckedShiftRL64 = I_shiftRight;
function hs_int64ToWord64(x) {
    var tmp = I_add(__w64_max, I_fromBits([x.getLowBits(), x.getHighBits()]));
    return I_rem(tmp, __w64_max);
}
function hs_word64ToInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

// Joseph Myers' MD5 implementation; used under the BSD license.

function md5cycle(x, k) {
var a = x[0], b = x[1], c = x[2], d = x[3];

a = ff(a, b, c, d, k[0], 7, -680876936);
d = ff(d, a, b, c, k[1], 12, -389564586);
c = ff(c, d, a, b, k[2], 17,  606105819);
b = ff(b, c, d, a, k[3], 22, -1044525330);
a = ff(a, b, c, d, k[4], 7, -176418897);
d = ff(d, a, b, c, k[5], 12,  1200080426);
c = ff(c, d, a, b, k[6], 17, -1473231341);
b = ff(b, c, d, a, k[7], 22, -45705983);
a = ff(a, b, c, d, k[8], 7,  1770035416);
d = ff(d, a, b, c, k[9], 12, -1958414417);
c = ff(c, d, a, b, k[10], 17, -42063);
b = ff(b, c, d, a, k[11], 22, -1990404162);
a = ff(a, b, c, d, k[12], 7,  1804603682);
d = ff(d, a, b, c, k[13], 12, -40341101);
c = ff(c, d, a, b, k[14], 17, -1502002290);
b = ff(b, c, d, a, k[15], 22,  1236535329);

a = gg(a, b, c, d, k[1], 5, -165796510);
d = gg(d, a, b, c, k[6], 9, -1069501632);
c = gg(c, d, a, b, k[11], 14,  643717713);
b = gg(b, c, d, a, k[0], 20, -373897302);
a = gg(a, b, c, d, k[5], 5, -701558691);
d = gg(d, a, b, c, k[10], 9,  38016083);
c = gg(c, d, a, b, k[15], 14, -660478335);
b = gg(b, c, d, a, k[4], 20, -405537848);
a = gg(a, b, c, d, k[9], 5,  568446438);
d = gg(d, a, b, c, k[14], 9, -1019803690);
c = gg(c, d, a, b, k[3], 14, -187363961);
b = gg(b, c, d, a, k[8], 20,  1163531501);
a = gg(a, b, c, d, k[13], 5, -1444681467);
d = gg(d, a, b, c, k[2], 9, -51403784);
c = gg(c, d, a, b, k[7], 14,  1735328473);
b = gg(b, c, d, a, k[12], 20, -1926607734);

a = hh(a, b, c, d, k[5], 4, -378558);
d = hh(d, a, b, c, k[8], 11, -2022574463);
c = hh(c, d, a, b, k[11], 16,  1839030562);
b = hh(b, c, d, a, k[14], 23, -35309556);
a = hh(a, b, c, d, k[1], 4, -1530992060);
d = hh(d, a, b, c, k[4], 11,  1272893353);
c = hh(c, d, a, b, k[7], 16, -155497632);
b = hh(b, c, d, a, k[10], 23, -1094730640);
a = hh(a, b, c, d, k[13], 4,  681279174);
d = hh(d, a, b, c, k[0], 11, -358537222);
c = hh(c, d, a, b, k[3], 16, -722521979);
b = hh(b, c, d, a, k[6], 23,  76029189);
a = hh(a, b, c, d, k[9], 4, -640364487);
d = hh(d, a, b, c, k[12], 11, -421815835);
c = hh(c, d, a, b, k[15], 16,  530742520);
b = hh(b, c, d, a, k[2], 23, -995338651);

a = ii(a, b, c, d, k[0], 6, -198630844);
d = ii(d, a, b, c, k[7], 10,  1126891415);
c = ii(c, d, a, b, k[14], 15, -1416354905);
b = ii(b, c, d, a, k[5], 21, -57434055);
a = ii(a, b, c, d, k[12], 6,  1700485571);
d = ii(d, a, b, c, k[3], 10, -1894986606);
c = ii(c, d, a, b, k[10], 15, -1051523);
b = ii(b, c, d, a, k[1], 21, -2054922799);
a = ii(a, b, c, d, k[8], 6,  1873313359);
d = ii(d, a, b, c, k[15], 10, -30611744);
c = ii(c, d, a, b, k[6], 15, -1560198380);
b = ii(b, c, d, a, k[13], 21,  1309151649);
a = ii(a, b, c, d, k[4], 6, -145523070);
d = ii(d, a, b, c, k[11], 10, -1120210379);
c = ii(c, d, a, b, k[2], 15,  718787259);
b = ii(b, c, d, a, k[9], 21, -343485551);

x[0] = add32(a, x[0]);
x[1] = add32(b, x[1]);
x[2] = add32(c, x[2]);
x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
a = add32(add32(a, q), add32(x, t));
return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s) {
var n = s.length,
state = [1732584193, -271733879, -1732584194, 271733878], i;
for (i=64; i<=s.length; i+=64) {
md5cycle(state, md5blk(s.substring(i-64, i)));
}
s = s.substring(i-64);
var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
for (i=0; i<s.length; i++)
tail[i>>2] |= s.charCodeAt(i) << ((i%4) << 3);
tail[i>>2] |= 0x80 << ((i%4) << 3);
if (i > 55) {
md5cycle(state, tail);
for (i=0; i<16; i++) tail[i] = 0;
}
tail[14] = n*8;
md5cycle(state, tail);
return state;
}

function md5blk(s) {
var md5blks = [], i;
for (i=0; i<64; i+=4) {
md5blks[i>>2] = s.charCodeAt(i)
+ (s.charCodeAt(i+1) << 8)
+ (s.charCodeAt(i+2) << 16)
+ (s.charCodeAt(i+3) << 24);
}
return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
var s='', j=0;
for(; j<4; j++)
s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
+ hex_chr[(n >> (j * 8)) & 0x0F];
return s;
}

function hex(x) {
for (var i=0; i<x.length; i++)
x[i] = rhex(x[i]);
return x.join('');
}

function md5(s) {
return hex(md51(s));
}

function add32(a, b) {
return (a + b) & 0xFFFFFFFF;
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// 2D Canvas drawing primitives.
function jsHasCtx2D(elem) {return !!elem.getContext;}
function jsGetCtx2D(elem) {return elem.getContext('2d');}
function jsBeginPath(ctx) {ctx.beginPath();}
function jsMoveTo(ctx, x, y) {ctx.moveTo(x, y);}
function jsLineTo(ctx, x, y) {ctx.lineTo(x, y);}
function jsStroke(ctx) {ctx.stroke();}
function jsFill(ctx) {ctx.fill();}
function jsRotate(ctx, radians) {ctx.rotate(radians);}
function jsTranslate(ctx, x, y) {ctx.translate(x, y);}
function jsScale(ctx, x, y) {ctx.scale(x, y);}
function jsPushState(ctx) {ctx.save();}
function jsPopState(ctx) {ctx.restore();}
function jsResetCanvas(el) {el.width = el.width;}
function jsDrawImage(ctx, img, x, y) {ctx.drawImage(img, x, y);}
function jsDrawImageClipped(ctx, img, x, y, cx, cy, cw, ch) {
    ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);
}
function jsDrawText(ctx, str, x, y) {ctx.fillText(str, x, y);}
function jsClip(ctx) {ctx.clip();}
function jsArc(ctx, x, y, radius, fromAngle, toAngle) {
    ctx.arc(x, y, radius, fromAngle, toAngle);
}
function jsCanvasToDataURL(el) {return el.toDataURL('image/png');}

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

var _0=function(_1,_2){return E(_2);},_3=function(_4,_5){return new F(function(){return _0(_4,_5);});},_6=function(_7,_8){return new F(function(){return A(_8,[_7]);});},_9=function(_a){return new F(function(){return err(_a);});},_b=function(_c){return E(_c);},_d=[0,_6,_3,_b,_9],_e=[0,34],_f=function(_g,_h){var _i=E(_g);return _i[0]==0?E(_h):[1,_i[1],new T(function(){return B(_f(_i[2],_h));})];},_j=function(_k){while(1){var _l=E(_k);if(!_l[0]){_k=[1,I_fromInt(_l[1])];continue;}else{return new F(function(){return I_toString(_l[1]);});}}},_m=function(_n,_o){return new F(function(){return _f(fromJSStr(B(_j(_n))),_o);});},_p=function(_q,_r){var _s=E(_q);if(!_s[0]){var _t=_s[1],_u=E(_r);return _u[0]==0?_t<_u[1]:I_compareInt(_u[1],_t)>0;}else{var _v=_s[1],_w=E(_r);return _w[0]==0?I_compareInt(_v,_w[1])<0:I_compare(_v,_w[1])<0;}},_x=[0,41],_y=[0,40],_z=[0,0],_A=function(_B,_C,_D){return _B<=6?B(_m(_C,_D)):!B(_p(_C,_z))?B(_m(_C,_D)):[1,_y,new T(function(){return B(_f(fromJSStr(B(_j(_C))),[1,_x,_D]));})];},_E=[0],_F=function(_G){return new F(function(){return _A(0,_G,_E);});},_H=[0,32],_I=[1,_H,_E],_J=new T(function(){return B(unCStr(" . "));}),_K=[0,41],_L=[1,_K,_E],_M=[1,_L,_E],_N=[1,_K,_E],_O=[1,_N,_E],_P=function(_Q,_R){var _S=E(_R);switch(_S[0]){case 0:return [1,new T(function(){return B(_T(_Q));}),[1,_I,new T(function(){return B(_P(_S[1],_S[2]));})]];case 6:return [1,new T(function(){return B(_T(_Q));}),_M];default:return [1,new T(function(){return B(_T(_Q));}),[1,_J,[1,new T(function(){return B(_T(_S));}),_O]]];}},_U=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_V=new T(function(){return B(err(_U));}),_W=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_X=new T(function(){return B(err(_W));}),_Y=function(_Z,_10){while(1){var _11=E(_Z);if(!_11[0]){return E(_X);}else{var _12=E(_10);if(!_12){return E(_11[1]);}else{_Z=_11[2];_10=_12-1|0;continue;}}}},_13=new T(function(){return B(unCStr("ACK"));}),_14=new T(function(){return B(unCStr("BEL"));}),_15=new T(function(){return B(unCStr("BS"));}),_16=new T(function(){return B(unCStr("SP"));}),_17=[1,_16,_E],_18=new T(function(){return B(unCStr("US"));}),_19=[1,_18,_17],_1a=new T(function(){return B(unCStr("RS"));}),_1b=[1,_1a,_19],_1c=new T(function(){return B(unCStr("GS"));}),_1d=[1,_1c,_1b],_1e=new T(function(){return B(unCStr("FS"));}),_1f=[1,_1e,_1d],_1g=new T(function(){return B(unCStr("ESC"));}),_1h=[1,_1g,_1f],_1i=new T(function(){return B(unCStr("SUB"));}),_1j=[1,_1i,_1h],_1k=new T(function(){return B(unCStr("EM"));}),_1l=[1,_1k,_1j],_1m=new T(function(){return B(unCStr("CAN"));}),_1n=[1,_1m,_1l],_1o=new T(function(){return B(unCStr("ETB"));}),_1p=[1,_1o,_1n],_1q=new T(function(){return B(unCStr("SYN"));}),_1r=[1,_1q,_1p],_1s=new T(function(){return B(unCStr("NAK"));}),_1t=[1,_1s,_1r],_1u=new T(function(){return B(unCStr("DC4"));}),_1v=[1,_1u,_1t],_1w=new T(function(){return B(unCStr("DC3"));}),_1x=[1,_1w,_1v],_1y=new T(function(){return B(unCStr("DC2"));}),_1z=[1,_1y,_1x],_1A=new T(function(){return B(unCStr("DC1"));}),_1B=[1,_1A,_1z],_1C=new T(function(){return B(unCStr("DLE"));}),_1D=[1,_1C,_1B],_1E=new T(function(){return B(unCStr("SI"));}),_1F=[1,_1E,_1D],_1G=new T(function(){return B(unCStr("SO"));}),_1H=[1,_1G,_1F],_1I=new T(function(){return B(unCStr("CR"));}),_1J=[1,_1I,_1H],_1K=new T(function(){return B(unCStr("FF"));}),_1L=[1,_1K,_1J],_1M=new T(function(){return B(unCStr("VT"));}),_1N=[1,_1M,_1L],_1O=new T(function(){return B(unCStr("LF"));}),_1P=[1,_1O,_1N],_1Q=new T(function(){return B(unCStr("HT"));}),_1R=[1,_1Q,_1P],_1S=[1,_15,_1R],_1T=[1,_14,_1S],_1U=[1,_13,_1T],_1V=new T(function(){return B(unCStr("ENQ"));}),_1W=[1,_1V,_1U],_1X=new T(function(){return B(unCStr("EOT"));}),_1Y=[1,_1X,_1W],_1Z=new T(function(){return B(unCStr("ETX"));}),_20=[1,_1Z,_1Y],_21=new T(function(){return B(unCStr("STX"));}),_22=[1,_21,_20],_23=new T(function(){return B(unCStr("SOH"));}),_24=[1,_23,_22],_25=new T(function(){return B(unCStr("NUL"));}),_26=[1,_25,_24],_27=[0,92],_28=new T(function(){return B(unCStr("\\DEL"));}),_29=new T(function(){return B(unCStr("\\a"));}),_2a=new T(function(){return B(unCStr("\\\\"));}),_2b=new T(function(){return B(unCStr("\\SO"));}),_2c=new T(function(){return B(unCStr("\\r"));}),_2d=new T(function(){return B(unCStr("\\f"));}),_2e=new T(function(){return B(unCStr("\\v"));}),_2f=new T(function(){return B(unCStr("\\n"));}),_2g=new T(function(){return B(unCStr("\\t"));}),_2h=new T(function(){return B(unCStr("\\b"));}),_2i=function(_2j,_2k){if(_2j<=127){var _2l=E(_2j);switch(_2l){case 92:return new F(function(){return _f(_2a,_2k);});break;case 127:return new F(function(){return _f(_28,_2k);});break;default:if(_2l<32){var _2m=E(_2l);switch(_2m){case 7:return new F(function(){return _f(_29,_2k);});break;case 8:return new F(function(){return _f(_2h,_2k);});break;case 9:return new F(function(){return _f(_2g,_2k);});break;case 10:return new F(function(){return _f(_2f,_2k);});break;case 11:return new F(function(){return _f(_2e,_2k);});break;case 12:return new F(function(){return _f(_2d,_2k);});break;case 13:return new F(function(){return _f(_2c,_2k);});break;case 14:return new F(function(){return _f(_2b,new T(function(){var _2n=E(_2k);if(!_2n[0]){var _2o=[0];}else{var _2o=E(E(_2n[1])[1])==72?B(unAppCStr("\\&",_2n)):E(_2n);}return _2o;}));});break;default:return new F(function(){return _f([1,_27,new T(function(){var _2p=_2m;return _2p>=0?B(_Y(_26,_2p)):E(_V);})],_2k);});}}else{return [1,[0,_2l],_2k];}}}else{return [1,_27,new T(function(){var _2q=jsShowI(_2j),_2r=_2q;return B(_f(fromJSStr(_2r),new T(function(){var _2s=E(_2k);if(!_2s[0]){var _2t=[0];}else{var _2u=E(_2s[1])[1],_2t=_2u<48?E(_2s):_2u>57?E(_2s):B(unAppCStr("\\&",_2s));}return _2t;})));})];}},_2v=function(_2w){var _2x=E(_2w);if(!_2x[0]){return [0];}else{return new F(function(){return _f(_2x[1],new T(function(){return B(_2v(_2x[2]));}));});}},_2y=[0,62],_2z=[1,_2y,_E],_2A=[0,40],_2B=[1,_2A,_E],_2C=[1,_e,_E],_2D=[0,35],_2E=[0,39],_2F=[1,_2E,_E],_2G=[1,_2y,_E],_2H=[1,_2y,_E],_2I=new T(function(){return B(unCStr("NIL"));}),_2J=new T(function(){return B(unCStr("true"));}),_2K=new T(function(){return B(unCStr("false"));}),_2L=new T(function(){return B(unCStr("\\\""));}),_2M=function(_2N,_2O){var _2P=E(_2N);if(!_2P[0]){return E(_2O);}else{var _2Q=_2P[2],_2R=E(E(_2P[1])[1]);if(_2R==34){return new F(function(){return _f(_2L,new T(function(){return B(_2M(_2Q,_2O));}));});}else{return new F(function(){return _2i(_2R,new T(function(){return B(_2M(_2Q,_2O));}));});}}},_2S=new T(function(){return B(unCStr("\'\\\'\'"));}),_T=function(_2T){var _2U=E(_2T);switch(_2U[0]){case 0:return new F(function(){return _2v([1,_2B,new T(function(){return B(_P(_2U[1],_2U[2]));})]);});break;case 1:return [1,_e,new T(function(){return B(_2M(_2U[1],_2C));})];case 2:return E(_2U[1]);case 3:return new F(function(){return _F(_2U[1]);});break;case 4:return !E(_2U[1])?E(_2K):E(_2J);case 5:return [1,_2D,new T(function(){var _2V=E(E(_2U[1])[1]);return _2V==39?E(_2S):[1,_2E,new T(function(){return B(_2i(_2V,_2F));})];})];case 6:return E(_2I);case 7:return new F(function(){return unAppCStr("<prim ",new T(function(){return B(_f(B(_T(_2U[2])),_2H));}));});break;case 8:return new F(function(){return unAppCStr("<fn ",new T(function(){return B(_f(B(_T(_2U[2])),_2G));}));});break;default:return new F(function(){return unAppCStr("<fexpr ",new T(function(){return B(_f(B(_T(_2U[2])),_2z));}));});}},_2W=function(_2X){return E(E(_2X)[1]);},_2Y=[0,1],_2Z=function(_30){return E(E(_30)[3]);},_31=function(_32,_33,_34){return [0,_32,E(E(_33)),_34];},_35=function(_36,_37,_38){var _39=new T(function(){return B(_2Z(_36));}),_3a=new T(function(){return B(_2Z(_36));});return new F(function(){return A(_37,[_38,function(_3b,_3c,_3d){return new F(function(){return A(_3a,[[0,new T(function(){return B(A(_39,[new T(function(){return B(_31(_3b,_3c,_3d));})]));})]]);});},function(_3e){return new F(function(){return A(_3a,[[0,new T(function(){return B(A(_39,[[1,_3e]]));})]]);});},function(_3f,_3g,_3h){return new F(function(){return A(_3a,[new T(function(){return [1,E(B(A(_39,[new T(function(){return B(_31(_3f,_3g,_3h));})])))];})]);});},function(_3i){return new F(function(){return A(_3a,[new T(function(){return [1,E(B(A(_39,[[1,_3i]])))];})]);});}]);});},_3j=function(_3k,_3l,_3m,_3n,_3o){var _3p=new T(function(){return B(_2Z(_3k));});return new F(function(){return A(_2W,[_3k,new T(function(){return B(_35(_3k,_3l,new T(function(){return [0,_3o,E([0,_3n,E(_2Y),E(_2Y)]),E(E(_3m))];})));}),function(_3q){return new F(function(){return A(new T(function(){return B(_2W(_3k));}),[new T(function(){var _3r=E(_3q);return _3r[0]==0?E(_3r[1]):E(_3r[1]);}),function(_3s){var _3t=E(_3s);return _3t[0]==0?B(A(_3p,[[1,_3t[1]]])):B(A(_3p,[[0,_3t[1]]]));}]);});}]);});},_3u=0,_3v=function(_3w){var _3x=E(_3w);if(!_3x[0]){return [0];}else{var _3y=_3x[2],_3z=E(_3x[1]);switch(E(_3z[1])){case 60:return new F(function(){return unAppCStr("&lt;",new T(function(){return B(_3v(_3y));}));});break;case 62:return new F(function(){return unAppCStr("&gt;",new T(function(){return B(_3v(_3y));}));});break;default:return [1,_3z,new T(function(){return B(_3v(_3y));})];}}},_3A=function(_3B,_3C){while(1){var _3D=E(_3B);if(!_3D[0]){return E(_3C)[0]==0?1:0;}else{var _3E=E(_3C);if(!_3E[0]){return 2;}else{var _3F=E(_3D[1])[1],_3G=E(_3E[1])[1];if(_3F!=_3G){return _3F>_3G?2:0;}else{_3B=_3D[2];_3C=_3E[2];continue;}}}}},_3H=[1],_3I=new T(function(){return B(unCStr("Failure in Data.Map.balanceL"));}),_3J=new T(function(){return B(err(_3I));}),_3K=function(_3L,_3M,_3N,_3O){var _3P=E(_3O);if(!_3P[0]){var _3Q=_3P[1],_3R=E(_3N);if(!_3R[0]){var _3S=_3R[1],_3T=_3R[2],_3U=_3R[3];if(_3S<=(imul(3,_3Q)|0)){return [0,(1+_3S|0)+_3Q|0,E(E(_3L)),_3M,E(_3R),E(_3P)];}else{var _3V=E(_3R[4]);if(!_3V[0]){var _3W=_3V[1],_3X=E(_3R[5]);if(!_3X[0]){var _3Y=_3X[1],_3Z=_3X[2],_40=_3X[3],_41=_3X[4];if(_3Y>=(imul(2,_3W)|0)){var _42=function(_43){var _44=E(_3X[5]);return _44[0]==0?[0,(1+_3S|0)+_3Q|0,E(_3Z),_40,E([0,(1+_3W|0)+_43|0,E(_3T),_3U,E(_3V),E(_41)]),E([0,(1+_3Q|0)+_44[1]|0,E(E(_3L)),_3M,E(_44),E(_3P)])]:[0,(1+_3S|0)+_3Q|0,E(_3Z),_40,E([0,(1+_3W|0)+_43|0,E(_3T),_3U,E(_3V),E(_41)]),E([0,1+_3Q|0,E(E(_3L)),_3M,E(_3H),E(_3P)])];},_45=E(_41);return _45[0]==0?B(_42(_45[1])):B(_42(0));}else{return [0,(1+_3S|0)+_3Q|0,E(_3T),_3U,E(_3V),E([0,(1+_3Q|0)+_3Y|0,E(E(_3L)),_3M,E(_3X),E(_3P)])];}}else{return E(_3J);}}else{return E(_3J);}}}else{return [0,1+_3Q|0,E(E(_3L)),_3M,E(_3H),E(_3P)];}}else{var _46=E(_3N);if(!_46[0]){var _47=_46[1],_48=_46[2],_49=_46[3],_4a=_46[5],_4b=E(_46[4]);if(!_4b[0]){var _4c=_4b[1],_4d=E(_4a);if(!_4d[0]){var _4e=_4d[1],_4f=_4d[2],_4g=_4d[3],_4h=_4d[4];if(_4e>=(imul(2,_4c)|0)){var _4i=function(_4j){var _4k=E(_4d[5]);return _4k[0]==0?[0,1+_47|0,E(_4f),_4g,E([0,(1+_4c|0)+_4j|0,E(_48),_49,E(_4b),E(_4h)]),E([0,1+_4k[1]|0,E(E(_3L)),_3M,E(_4k),E(_3H)])]:[0,1+_47|0,E(_4f),_4g,E([0,(1+_4c|0)+_4j|0,E(_48),_49,E(_4b),E(_4h)]),E([0,1,E(E(_3L)),_3M,E(_3H),E(_3H)])];},_4l=E(_4h);return _4l[0]==0?B(_4i(_4l[1])):B(_4i(0));}else{return [0,1+_47|0,E(_48),_49,E(_4b),E([0,1+_4e|0,E(E(_3L)),_3M,E(_4d),E(_3H)])];}}else{return [0,3,E(_48),_49,E(_4b),E([0,1,E(E(_3L)),_3M,E(_3H),E(_3H)])];}}else{var _4m=E(_4a);return _4m[0]==0?[0,3,E(_4m[2]),_4m[3],E([0,1,E(_48),_49,E(_3H),E(_3H)]),E([0,1,E(E(_3L)),_3M,E(_3H),E(_3H)])]:[0,2,E(E(_3L)),_3M,E(_46),E(_3H)];}}else{return [0,1,E(E(_3L)),_3M,E(_3H),E(_3H)];}}},_4n=new T(function(){return B(unCStr("Failure in Data.Map.balanceR"));}),_4o=new T(function(){return B(err(_4n));}),_4p=function(_4q,_4r,_4s,_4t){var _4u=E(_4s);if(!_4u[0]){var _4v=_4u[1],_4w=E(_4t);if(!_4w[0]){var _4x=_4w[1],_4y=_4w[2],_4z=_4w[3];if(_4x<=(imul(3,_4v)|0)){return [0,(1+_4v|0)+_4x|0,E(E(_4q)),_4r,E(_4u),E(_4w)];}else{var _4A=E(_4w[4]);if(!_4A[0]){var _4B=_4A[1],_4C=_4A[2],_4D=_4A[3],_4E=_4A[4],_4F=E(_4w[5]);if(!_4F[0]){var _4G=_4F[1];if(_4B>=(imul(2,_4G)|0)){var _4H=function(_4I){var _4J=E(_4q),_4K=E(_4A[5]);return _4K[0]==0?[0,(1+_4v|0)+_4x|0,E(_4C),_4D,E([0,(1+_4v|0)+_4I|0,E(_4J),_4r,E(_4u),E(_4E)]),E([0,(1+_4G|0)+_4K[1]|0,E(_4y),_4z,E(_4K),E(_4F)])]:[0,(1+_4v|0)+_4x|0,E(_4C),_4D,E([0,(1+_4v|0)+_4I|0,E(_4J),_4r,E(_4u),E(_4E)]),E([0,1+_4G|0,E(_4y),_4z,E(_3H),E(_4F)])];},_4L=E(_4E);return _4L[0]==0?B(_4H(_4L[1])):B(_4H(0));}else{return [0,(1+_4v|0)+_4x|0,E(_4y),_4z,E([0,(1+_4v|0)+_4B|0,E(E(_4q)),_4r,E(_4u),E(_4A)]),E(_4F)];}}else{return E(_4o);}}else{return E(_4o);}}}else{return [0,1+_4v|0,E(E(_4q)),_4r,E(_4u),E(_3H)];}}else{var _4M=E(_4t);if(!_4M[0]){var _4N=_4M[1],_4O=_4M[2],_4P=_4M[3],_4Q=_4M[5],_4R=E(_4M[4]);if(!_4R[0]){var _4S=_4R[1],_4T=_4R[2],_4U=_4R[3],_4V=_4R[4],_4W=E(_4Q);if(!_4W[0]){var _4X=_4W[1];if(_4S>=(imul(2,_4X)|0)){var _4Y=function(_4Z){var _50=E(_4q),_51=E(_4R[5]);return _51[0]==0?[0,1+_4N|0,E(_4T),_4U,E([0,1+_4Z|0,E(_50),_4r,E(_3H),E(_4V)]),E([0,(1+_4X|0)+_51[1]|0,E(_4O),_4P,E(_51),E(_4W)])]:[0,1+_4N|0,E(_4T),_4U,E([0,1+_4Z|0,E(_50),_4r,E(_3H),E(_4V)]),E([0,1+_4X|0,E(_4O),_4P,E(_3H),E(_4W)])];},_52=E(_4V);return _52[0]==0?B(_4Y(_52[1])):B(_4Y(0));}else{return [0,1+_4N|0,E(_4O),_4P,E([0,1+_4S|0,E(E(_4q)),_4r,E(_3H),E(_4R)]),E(_4W)];}}else{return [0,3,E(_4T),_4U,E([0,1,E(E(_4q)),_4r,E(_3H),E(_3H)]),E([0,1,E(_4O),_4P,E(_3H),E(_3H)])];}}else{var _53=E(_4Q);return _53[0]==0?[0,3,E(_4O),_4P,E([0,1,E(E(_4q)),_4r,E(_3H),E(_3H)]),E(_53)]:[0,2,E(E(_4q)),_4r,E(_3H),E(_4M)];}}else{return [0,1,E(E(_4q)),_4r,E(_3H),E(_3H)];}}},_54=function(_55,_56,_57){var _58=E(_55),_59=E(_57);if(!_59[0]){var _5a=_59[2],_5b=_59[3],_5c=_59[4],_5d=_59[5];switch(B(_3A(_58,_5a))){case 0:return new F(function(){return _3K(_5a,_5b,B(_54(_58,_56,_5c)),_5d);});break;case 1:return [0,_59[1],E(_58),_56,E(_5c),E(_5d)];default:return new F(function(){return _4p(_5a,_5b,_5c,B(_54(_58,_56,_5d)));});}}else{return [0,1,E(_58),_56,E(_3H),E(_3H)];}},_5e=[6],_5f=function(_5g,_5h,_5i){var _5j=E(_5g);return _5j[0]==0?[0]:[1,new T(function(){return B(_54(_5h,_5i,_5j[1]));}),_5j[2]];},_5k=function(_5l){return new F(function(){return err(B(unAppCStr("Too many arguments: ",new T(function(){return B(_T(_5l));}))));});},_5m=function(_5n){return new F(function(){return err(B(unAppCStr("Too few arguments: still expecting ",new T(function(){return B(_T(_5n));}))));});},_5o=new T(function(){return B(unCStr("Something odd happened"));}),_5p=[1,_2E,_E],_5q=function(_5r){var _5s=E(E(_5r)[1]);return _5s==39?E(_2S):[1,_2E,new T(function(){return B(_2i(_5s,_5p));})];},_5t=function(_5u,_5v){return [1,_e,new T(function(){return B(_2M(_5u,[1,_e,_5v]));})];},_5w=function(_5x){return new F(function(){return _f(_2S,_5x);});},_5y=function(_5z,_5A){var _5B=E(E(_5A)[1]);return _5B==39?E(_5w):function(_5C){return [1,_2E,new T(function(){return B(_2i(_5B,[1,_2E,_5C]));})];};},_5D=[0,_5y,_5q,_5t],_5E=function(_5F){return E(E(_5F)[3]);},_5G=function(_5H,_5I){return new F(function(){return A(_5E,[_5H,_5I,_E]);});},_5J=[0,44],_5K=[0,93],_5L=[0,91],_5M=function(_5N,_5O,_5P){var _5Q=E(_5O);return _5Q[0]==0?B(unAppCStr("[]",_5P)):[1,_5L,new T(function(){return B(A(_5N,[_5Q[1],new T(function(){var _5R=function(_5S){var _5T=E(_5S);return _5T[0]==0?E([1,_5K,_5P]):[1,_5J,new T(function(){return B(A(_5N,[_5T[1],new T(function(){return B(_5R(_5T[2]));})]));})];};return B(_5R(_5Q[2]));})]));})];},_5U=function(_5V,_5W,_5X){return new F(function(){return _5M(new T(function(){return B(_5E(_5V));}),_5W,_5X);});},_5Y=function(_5Z){return [0,function(_60){return E(new T(function(){return B(_5E(_5Z));}));},function(_5x){return new F(function(){return _5G(_5Z,_5x);});},function(_61,_5x){return new F(function(){return _5U(_5Z,_61,_5x);});}];},_62=new T(function(){return B(_5Y(_5D));}),_63=function(_64,_65){return new F(function(){return _f(B(_T(_64)),_65);});},_66=function(_67,_68){return new F(function(){return _5M(_63,_67,_68);});},_69=function(_6a,_6b,_6c){return new F(function(){return _f(B(_T(_6b)),_6c);});},_6d=[0,_69,_T,_66],_6e=function(_6f,_6g){while(1){var _6h=(function(_6i,_6j){var _6k=E(_6j);if(!_6k[0]){_6f=[1,[0,_6k[2],_6k[3]],new T(function(){return B(_6e(_6i,_6k[5]));})];_6g=_6k[4];return null;}else{return E(_6i);}})(_6f,_6g);if(_6h!=null){return _6h;}}},_6l=function(_6m,_6n,_6o){return new F(function(){return A(_6m,[[1,_5J,new T(function(){return B(A(_6n,[_6o]));})]]);});},_6p=new T(function(){return B(unCStr(": empty list"));}),_6q=new T(function(){return B(unCStr("Prelude."));}),_6r=function(_6s){return new F(function(){return err(B(_f(_6q,new T(function(){return B(_f(_6s,_6p));}))));});},_6t=new T(function(){return B(unCStr("foldr1"));}),_6u=new T(function(){return B(_6r(_6t));}),_6v=function(_6w,_6x){var _6y=E(_6x);if(!_6y[0]){return E(_6u);}else{var _6z=_6y[1],_6A=E(_6y[2]);if(!_6A[0]){return E(_6z);}else{return new F(function(){return A(_6w,[_6z,new T(function(){return B(_6v(_6w,_6A));})]);});}}},_6B=[0,0],_6C=function(_6D){return E(E(_6D)[1]);},_6E=function(_6F,_6G,_6H,_6I,_6J){return [1,_y,new T(function(){return B(A(_6v,[_6l,[1,new T(function(){return B(A(_6C,[_6F,_6B,_6H]));}),[1,new T(function(){return B(A(_6C,[_6G,_6B,_6I]));}),_E]],[1,_x,_6J]]));})];},_6K=function(_6L,_6M,_6N,_6O){return new F(function(){return _5M(function(_6P,_6Q){var _6R=E(_6P);return new F(function(){return _6E(_6L,_6M,_6R[1],_6R[2],_6Q);});},_6N,_6O);});},_6S=new T(function(){return B(unCStr("fromList "));}),_6T=function(_6U,_6V,_6W,_6X){var _6Y=new T(function(){return B(_6e(_E,_6X));});return _6W<=10?function(_6Z){return new F(function(){return _f(_6S,new T(function(){return B(_6K(_6U,_6V,_6Y,_6Z));}));});}:function(_70){return [1,_y,new T(function(){return B(_f(_6S,new T(function(){return B(_6K(_6U,_6V,_6Y,[1,_x,_70]));})));})];};},_71=function(_72){return new F(function(){return _6T(_62,_6d,0,_72);});},_73=[0,10],_74=function(_75){var _76=E(_75);if(!_76[0]){return [0];}else{return new F(function(){return _f(_76[1],[1,_73,new T(function(){return B(_74(_76[2]));})]);});}},_77=function(_78,_79,_7a){return new F(function(){return err(B(_74([1,_5o,[1,new T(function(){return B(_T(_79));}),[1,new T(function(){return B(_T(_7a));}),[1,new T(function(){return B(_5M(_71,_78,_E));}),_E]]]])));});},_7b=function(_7c,_7d,_7e){var _7f=new T(function(){var _7g=E(_7e);if(_7g[0]==6){var _7h=B(_5m(_7d));}else{var _7i=E(_7d);switch(_7i[0]){case 0:var _7j=E(_7i[1]);if(_7j[0]==2){var _7k=E(_7g);if(!_7k[0]){var _7l=B(_7b(B(_5f(_7c,_7j[1],_7k[1])),_7i[2],_7k[2]));}else{var _7l=B(_77(_7c,_7i,_7k));}var _7m=_7l;}else{var _7m=B(_77(_7c,_7i,_7g));}var _7n=_7m;break;case 6:var _7n=B(_5k(_7g));break;default:var _7n=B(_77(_7c,_7i,_7g));}var _7h=_7n;}return _7h;});return E(_7d)[0]==6?E(_7e)[0]==6?E(_7c):E(_7f):E(_7f);},_7o=function(_7p){return new F(function(){return err(B(unAppCStr("Called eval_args on a non-cell: ",new T(function(){return B(_T(_7p));}))));});},_7q=function(_7r,_7s){var _7t=E(_7r);switch(_7t[0]){case 0:return [0,new T(function(){var _7u=B(_7v(_7t[1],_7s));return _7u[0]==0?E(_7u[1]):E(_7u[1]);}),new T(function(){return B(_7q(_7t[2],_7s));})];case 6:return [6];default:return new F(function(){return _7o(_7t);});}},_7w=function(_7x){return new F(function(){return err(B(unAppCStr("Called eval_sequence on a non-cell: ",new T(function(){return B(_T(_7x));}))));});},_7y=function(_7z,_7A){while(1){var _7B=(function(_7C,_7D){var _7E=E(_7C);if(!_7E[0]){var _7F=_7E[1],_7G=E(_7E[2]);if(_7G[0]==6){return new F(function(){return _7v(_7F,_7D);});}else{_7z=_7G;_7A=new T(function(){var _7H=B(_7v(_7F,_7D));return _7H[0]==0?E(_7D):E(_7H[2]);});return null;}}else{return new F(function(){return _7w(_7E);});}})(_7z,_7A);if(_7B!=null){return _7B;}}},_7I=[0,39],_7J=[1,_7I,_E],_7K=function(_7L){return new F(function(){return err(B(unAppCStr("Undefined function \'",new T(function(){return B(_f(B(_T(_7L)),_7J));}))));});},_7M=function(_7N,_7O,_7P){var _7Q=function(_7R,_7S,_7T){var _7U=B(_7y(_7T,new T(function(){return B(_7b([1,_3H,_7R],_7S,_7O));})));if(!_7U[0]){var _7V=B(_7v(_7U[1],_7P));return _7V[0]==0?E(_7V[1]):E(_7V[1]);}else{var _7W=B(_7v(_7U[1],_7P));return _7W[0]==0?E(_7W[1]):E(_7W[1]);}},_7X=function(_7Y,_7Z,_80){var _81=B(_7y(_80,new T(function(){return B(_7b([1,_3H,_7Y],_7Z,B(_7q(_7O,_7P))));})));return _81[0]==0?E(_81[1]):E(_81[1]);},_82=function(_83,_84){return new F(function(){return A(_83,[new T(function(){return B(_7b([1,_3H,_7P],_84,B(_7q(_7O,_7P))));})]);});},_85=B(_7v(_7N,_7P));if(!_85[0]){var _86=E(_85[1]);switch(_86[0]){case 7:return new F(function(){return _82(_86[1],_86[2]);});break;case 8:return new F(function(){return _7X(_86[1],_86[2],_86[3]);});break;case 9:return new F(function(){return _7Q(_86[1],_86[2],_86[3]);});break;default:return new F(function(){return _7K(_7N);});}}else{var _87=E(_85[1]);switch(_87[0]){case 7:return new F(function(){return _82(_87[1],_87[2]);});break;case 8:return new F(function(){return _7X(_87[1],_87[2],_87[3]);});break;case 9:return new F(function(){return _7Q(_87[1],_87[2],_87[3]);});break;default:return new F(function(){return _7K(_7N);});}}},_88=function(_89,_8a){while(1){var _8b=E(_89);if(!_8b[0]){return E(_8a)[0]==0?true:false;}else{var _8c=E(_8a);if(!_8c[0]){return false;}else{if(E(_8b[1])[1]!=E(_8c[1])[1]){return false;}else{_89=_8b[2];_8a=_8c[2];continue;}}}}},_8d=function(_8e,_8f){while(1){var _8g=E(_8e),_8h=E(_8f);if(!_8h[0]){switch(B(_3A(_8g,_8h[2]))){case 0:_8e=_8g;_8f=_8h[4];continue;case 1:return [1,_8h[3]];default:_8e=_8g;_8f=_8h[5];continue;}}else{return [0];}}},_8i=function(_8j,_8k){while(1){var _8l=E(_8j);if(!_8l[0]){return [6];}else{var _8m=B(_8d(_8k,_8l[1]));if(!_8m[0]){_8j=_8l[2];continue;}else{return E(_8m[1]);}}}},_8n=new T(function(){return B(unCStr("def"));}),_8o=new T(function(){return B(unCStr("do"));}),_8p=function(_8q){return new F(function(){return err(B(unAppCStr("EVAL :: invalid form ",new T(function(){return B(_T(_8q));}))));});},_8r=function(_8s){return new F(function(){return err(B(unAppCStr("Tried to assign to non-symbol: ",new T(function(){return B(_T(_8s));}))));});},_8t=new T(function(){return B(unCStr("\' ..."));}),_8u=function(_8v){return new F(function(){return err(B(unAppCStr("Tried to assign to unbound symbol \'",new T(function(){return B(_f(_8v,_8t));}))));});},_8w=function(_8x){return new F(function(){return err(B(unAppCStr("Tried to bind to non-symbol: ",new T(function(){return B(_T(_8x));}))));});},_8y=new T(function(){return B(unCStr("\' already bound ..."));}),_8z=function(_8A){return new F(function(){return err(B(unAppCStr("Symbol \'",new T(function(){return B(_f(_8A,_8y));}))));});},_8B=[0,_5e],_8C=new T(function(){return B(unCStr("fexpr"));}),_8D=new T(function(){return B(unCStr("fn"));}),_8E=new T(function(){return B(unCStr("if"));}),_8F=new T(function(){return B(unCStr("quote"));}),_8G=new T(function(){return B(unCStr("set!"));}),_7v=function(_8H,_8I){var _8J=E(_8H);switch(_8J[0]){case 0:var _8K=_8J[2],_8L=E(_8J[1]);if(_8L[0]==2){var _8M=_8L[1],_8N=new T(function(){if(!B(_88(_8M,_8o))){var _8O=new T(function(){var _8P=new T(function(){var _8Q=new T(function(){if(!B(_88(_8M,_8F))){if(!B(_88(_8M,_8G))){var _8R=[0,new T(function(){return B(_7M(_8L,_8K,_8I));})];}else{var _8S=E(_8K);if(!_8S[0]){var _8T=E(_8S[2]);if(!_8T[0]){if(E(_8T[2])[0]==6){var _8U=E(_8S[1]);if(_8U[0]==2){var _8V=_8U[1];if(B(_8i(_8I,_8V))[0]==6){var _8W=new T(function(){return B(_7v(_8T[1],_8I));}),_8X=new T(function(){var _8Y=E(_8W);return _8Y[0]==0?E(_8Y[1]):E(_8Y[1]);}),_8Z=[1,_8X,new T(function(){var _90=E(_8W);if(!_90[0]){var _91=E(_8I),_92=_91[0]==0?[0]:[1,new T(function(){return B(_54(_8V,_8X,_91[1]));}),_91[2]];}else{var _93=E(_90[2]),_92=_93[0]==0?[0]:[1,new T(function(){return B(_54(_8V,_8X,_93[1]));}),_93[2]];}return _92;})];}else{var _8Z=B(_8u(_8V));}var _94=_8Z;}else{var _94=B(_8r(_8U));}var _95=_94;}else{var _95=[0,new T(function(){return B(_7M(_8L,_8S,_8I));})];}var _96=_95;}else{var _96=[0,new T(function(){return B(_7M(_8L,_8S,_8I));})];}var _97=_96;}else{var _97=[0,new T(function(){return B(_7M(_8L,_8S,_8I));})];}var _8R=_97;}var _98=_8R;}else{var _98=[0,_8K];}return _98;});if(!B(_88(_8M,_8E))){var _99=E(_8Q);}else{var _9a=E(_8K);if(!_9a[0]){var _9b=E(_9a[2]);if(!_9b[0]){var _9c=_9b[1],_9d=E(_9b[2]);if(!_9d[0]){var _9e=_9d[1];if(E(_9d[2])[0]==6){var _9f=B(_7v(_9a[1],_8I));if(!_9f[0]){var _9g=E(_9f[1]),_9h=_9g[0]==4?!E(_9g[1])?B(_7v(_9e,_8I)):B(_7v(_9c,_8I)):B(_7v(_9c,_8I));}else{var _9i=E(_9f[1]),_9h=_9i[0]==4?!E(_9i[1])?B(_7v(_9e,_8I)):B(_7v(_9c,_8I)):B(_7v(_9c,_8I));}var _9j=_9h;}else{var _9j=E(_8Q);}var _9k=_9j;}else{var _9k=E(_8Q);}var _9l=_9k;}else{var _9l=E(_8Q);}var _9m=_9l;}else{var _9m=E(_8Q);}var _99=_9m;}return _99;});if(!B(_88(_8M,_8D))){var _9n=E(_8P);}else{var _9o=E(_8K),_9n=_9o[0]==0?[0,[8,_8I,_9o[1],_9o[2]]]:E(_8P);}return _9n;});if(!B(_88(_8M,_8C))){var _9p=E(_8O);}else{var _9q=E(_8K),_9p=_9q[0]==0?[0,[9,_8I,_9q[1],_9q[2]]]:E(_8O);}var _9r=_9p;}else{var _9r=B(_7y(_8K,_8I));}return _9r;});if(!B(_88(_8M,_8n))){return E(_8N);}else{var _9s=E(_8K);if(!_9s[0]){var _9t=E(_9s[2]);if(!_9t[0]){if(E(_9t[2])[0]==6){var _9u=E(_9s[1]);if(_9u[0]==2){var _9v=_9u[1];if(B(_8i(_8I,_9v))[0]==6){return [1,_5e,new T(function(){var _9w=new T(function(){var _9x=B(_7v(_9t[1],_8I)),_9y=function(_9z,_9A){return [1,new T(function(){return B(_54(_9v,new T(function(){var _9B=E(_9x);if(!_9B[0]){var _9C=E(_9B[1]);switch(_9C[0]){case 8:var _9D=[8,_9w,_9C[2],_9C[3]];break;case 9:var _9D=[9,_9w,_9C[2],_9C[3]];break;default:var _9D=E(_9C);}var _9E=_9D;}else{var _9F=E(_9B[1]);switch(_9F[0]){case 8:var _9G=[8,_9w,_9F[2],_9F[3]];break;case 9:var _9G=[9,_9w,_9F[2],_9F[3]];break;default:var _9G=E(_9F);}var _9E=_9G;}return _9E;}),_9z));}),_9A];},_9H=E(_9x);if(!_9H[0]){var _9I=E(_8I),_9J=_9I[0]==0?[0]:B(_9y(_9I[1],_9I[2]));}else{var _9K=E(_9H[2]),_9J=_9K[0]==0?[0]:B(_9y(_9K[1],_9K[2]));}var _9L=_9J;return _9L;});return E(_9w);})];}else{return new F(function(){return _8z(_9v);});}}else{return new F(function(){return _8w(_9u);});}}else{return E(_8N);}}else{return E(_8N);}}else{return E(_8N);}}}else{return [0,new T(function(){return B(_7M(_8L,_8K,_8I));})];}break;case 1:return [0,_8J];case 2:return [0,new T(function(){return B(_8i(_8I,_8J[1]));})];case 3:return [0,_8J];case 4:return [0,_8J];case 5:return [0,_8J];case 6:return E(_8B);default:return new F(function(){return _8p(_8J);});}},_9M=new T(function(){return B(unCStr("\n   => "));}),_9N=new T(function(){return B(unCStr("Read error ..."));}),_9O=new T(function(){return B(unCStr("repl-log"));}),_9P=function(_9Q,_9R,_){var _9S=jsGet(_9Q,toJSStr(E(_9R))),_9T=_9S;return new T(function(){return fromJSStr(_9T);});},_9U=function(_9V,_9W,_9X,_9Y){return new F(function(){return A(_9V,[new T(function(){return function(_){var _9Z=jsSet(E(_9W)[1],toJSStr(E(_9X)),toJSStr(E(_9Y)));return _3u;};})]);});},_a0=new T(function(){return B(unCStr("\n\n"));}),_a1=new T(function(){return B(unCStr("innerHTML"));}),_a2=function(_a3){return E(_a3);},_a4=new T(function(){return B(unCStr(" could be found!"));}),_a5=function(_a6){return new F(function(){return err(B(unAppCStr("No element with ID ",new T(function(){return B(_f(_a6,_a4));}))));});},_a7=function(_a8,_a9,_){var _aa=E(_a8),_ab=jsFind(toJSStr(_aa)),_ac=_ab,_ad=E(_ac);if(!_ad[0]){return new F(function(){return _a5(_aa);});}else{var _ae=E(_ad[1]),_af=B(_9P(_ae[1],_a1,_)),_ag=_af,_ah=B(A(_9U,[_a2,_ae,_a1,new T(function(){return B(_f(_a9,new T(function(){return B(_f(_a0,_ag));})));}),_])),_ai=_ah;return _3u;}},_aj=new T(function(){return [0,"value"];}),_ak=new T(function(){return B(unCStr("lisp"));}),_al=new T(function(){return B(_f(_E,_E));}),_am=function(_an,_ao,_ap,_aq){var _ar=function(_){var _as=E(_ao);if(!_as[0]){var _at=E(_aq);if(!_at[0]){var _au=E(_an),_av=E(_au[2])[1],_aw=E(_au[3])[1],_ax=E(_ap),_ay=E(_ax[2])[1],_az=E(_ax[3])[1];switch(B(_3A(_au[1],_ax[1]))){case 0:return [0,_ax,_E];case 1:return _av>=_ay?_av!=_ay?[0,_au,_E]:_aw>=_az?_aw!=_az?[0,_au,_E]:[0,_au,_al]:[0,_ax,_E]:[0,_ax,_E];default:return [0,_au,_E];}}else{return [0,_ap,_at];}}else{var _aA=E(_an),_aB=E(_aA[2])[1],_aC=E(_aA[3])[1],_aD=E(_ap),_aE=E(_aD[2])[1],_aF=E(_aD[3])[1];switch(B(_3A(_aA[1],_aD[1]))){case 0:return [0,_aD,_aq];case 1:return _aB>=_aE?_aB!=_aE?[0,_aA,_as]:_aC>=_aF?_aC!=_aF?[0,_aA,_as]:[0,_aA,new T(function(){return B(_f(_as,_aq));})]:[0,_aD,_aq]:[0,_aD,_aq];default:return [0,_aA,_as];}}};if(!E(_aq)[0]){var _aG=E(_ao);return _aG[0]==0?B(_ar(_)):[0,_an,_aG];}else{return new F(function(){return _ar(_);});}},_aH=new T(function(){return B(unCStr("Text.ParserCombinators.Parsec.Prim.many: combinator \'many\' is applied to a parser that accepts an empty string."));}),_aI=new T(function(){return B(err(_aH));}),_aJ=function(_aK,_aL,_aM,_aN,_aO){var _aP=function(_aQ,_aR){return new F(function(){return A(_aK,[_aR,new T(function(){var _aS=E(_aQ);return function(_aT,_aU,_aV){return new F(function(){return _aP(_E,_aU);});};}),_aN,_aI,function(_aW){return new F(function(){return A(_aM,[_3u,_aR,new T(function(){var _aX=E(_aW),_aY=B(_am(_aX[1],_aX[2],E(_aR)[2],_E));return [0,E(_aY[1]),_aY[2]];})]);});}]);});};return new F(function(){return A(_aK,[_aL,function(_aZ,_b0,_b1){return new F(function(){return _aP(_E,_b0);});},_aN,_aI,function(_b2){return new F(function(){return A(_aO,[_3u,_aL,new T(function(){var _b3=E(_b2),_b4=B(_am(_b3[1],_b3[2],E(_aL)[2],_E));return [0,E(_b4[1]),_b4[2]];})]);});}]);});},_b5=function(_b6,_b7){while(1){var _b8=E(_b6);if(!_b8[0]){return E(_b7);}else{_b6=_b8[2];var _b9=[1,_b8[1],_b7];_b7=_b9;continue;}}},_ba=function(_bb){return function(_bc,_bd,_be,_bf,_bg){return new F(function(){return A(_bf,[new T(function(){return B(_b5(_bb,_E));}),_bc,new T(function(){return [0,E(E(_bc)[2]),_E];})]);});};},_bh=function(_bi,_bj){var _bk=E(_bi),_bl=E(_bj),_bm=B(_am(_bk[1],_bk[2],_bl[1],_bl[2]));return [0,E(_bm[1]),_bm[2]];},_bn=function(_bo,_bp,_bq,_br,_bs,_bt){var _bu=function(_bv,_bw,_bx){var _by=[1,_bw,_bv];return new F(function(){return A(_bo,[_bx,new T(function(){var _bz=E(_bv);return function(_bA,_bB,_bC){return new F(function(){return _bu(_by,_bA,_bB);});};}),_br,_aI,function(_bD){return new F(function(){return A(_ba,[_by,_bx,_bq,_br,function(_bE,_bF,_bG){return new F(function(){return A(_bq,[_bE,_bF,new T(function(){return B(_bh(_bD,_bG));})]);});},function(_bH){return new F(function(){return A(_br,[new T(function(){return B(_bh(_bD,_bH));})]);});}]);});}]);});};return new F(function(){return A(_bo,[_bp,function(_bI,_bJ,_bK){return new F(function(){return _bu(_E,_bI,_bJ);});},_br,_aI,function(_bL){return new F(function(){return A(_ba,[_E,_bp,_bq,_br,function(_bM,_bN,_bO){return new F(function(){return A(_bs,[_bM,_bN,new T(function(){return B(_bh(_bL,_bO));})]);});},function(_bP){return new F(function(){return A(_bt,[new T(function(){return B(_bh(_bL,_bP));})]);});}]);});}]);});},_bQ=function(_bR,_bS,_bT,_bU,_bV,_bW){var _bX=function(_bY,_bZ,_c0,_c1,_c2,_c3){return new F(function(){return _bn(_bR,_bZ,function(_c4,_c5,_c6){return new F(function(){return A(_c0,[[1,_bY,_c4],_c5,new T(function(){var _c7=E(_c6),_c8=B(_am(_c7[1],_c7[2],E(_c5)[2],_E));return [0,E(_c8[1]),_c8[2]];})]);});},_c1,function(_c9,_ca,_cb){return new F(function(){return A(_c2,[[1,_bY,_c9],_ca,new T(function(){var _cc=E(_cb),_cd=B(_am(_cc[1],_cc[2],E(_ca)[2],_E));return [0,E(_cd[1]),_cd[2]];})]);});},_c3);});};return new F(function(){return A(_bR,[_bS,function(_ce,_cf,_cg){return new F(function(){return _bX(_ce,_cf,_bT,_bU,function(_ch,_ci,_cj){return new F(function(){return A(_bT,[_ch,_ci,new T(function(){return B(_bh(_cg,_cj));})]);});},function(_ck){return new F(function(){return A(_bU,[new T(function(){return B(_bh(_cg,_ck));})]);});});});},_bU,function(_cl,_cm,_cn){return new F(function(){return _bX(_cl,_cm,_bT,_bU,function(_co,_cp,_cq){return new F(function(){return A(_bV,[_co,_cp,new T(function(){return B(_bh(_cn,_cq));})]);});},function(_cr){return new F(function(){return A(_bW,[new T(function(){return B(_bh(_cn,_cr));})]);});});});},_bW]);});},_cs=function(_ct){return [2,E(E(_ct))];},_cu=function(_cv,_cw){switch(E(_cv)[0]){case 0:switch(E(_cw)[0]){case 0:return false;case 1:return true;case 2:return true;default:return true;}break;case 1:switch(E(_cw)[0]){case 0:return true;case 1:return false;case 2:return true;default:return true;}break;case 2:switch(E(_cw)[0]){case 0:return true;case 1:return true;case 2:return false;default:return true;}break;default:switch(E(_cw)[0]){case 0:return true;case 1:return true;case 2:return true;default:return false;}}},_cx=[2,E(_E)],_cy=function(_cz){return new F(function(){return _cu(_cx,_cz);});},_cA=function(_cB,_cC){while(1){var _cD=(function(_cE,_cF){var _cG=E(_cF);if(!_cG[0]){return [0];}else{var _cH=_cG[1],_cI=_cG[2];if(!B(A(_cE,[_cH]))){var _cJ=_cE;_cC=_cI;_cB=_cJ;return null;}else{return [1,_cH,new T(function(){return B(_cA(_cE,_cI));})];}}})(_cB,_cC);if(_cD!=null){return _cD;}}},_cK=function(_cL,_cM,_cN){var _cO=E(_cN);if(!_cO[0]){return [0,_cL,[1,_cx,new T(function(){return B(_cA(_cy,_cM));})]];}else{var _cP=_cO[1],_cQ=E(_cO[2]);if(!_cQ[0]){var _cR=new T(function(){return [2,E(E(_cP))];});return [0,_cL,[1,_cR,new T(function(){return B(_cA(function(_cz){return new F(function(){return _cu(_cR,_cz);});},_cM));})]];}else{var _cS=new T(function(){return [2,E(E(_cP))];}),_cT=function(_cU){var _cV=E(_cU);if(!_cV[0]){return [0,_cL,[1,_cS,new T(function(){return B(_cA(function(_cz){return new F(function(){return _cu(_cS,_cz);});},_cM));})]];}else{var _cW=B(_cT(_cV[2]));return [0,_cW[1],[1,new T(function(){return B(_cs(_cV[1]));}),_cW[2]]];}};return new F(function(){return (function(_cX,_cY){var _cZ=B(_cT(_cY));return [0,_cZ[1],[1,new T(function(){return B(_cs(_cX));}),_cZ[2]]];})(_cQ[1],_cQ[2]);});}}},_d0=function(_d1,_d2){var _d3=E(_d1),_d4=B(_cK(_d3[1],_d3[2],_d2));return [0,E(_d4[1]),_d4[2]];},_d5=function(_d6,_d7,_d8,_d9,_da,_db,_dc){return new F(function(){return A(_d6,[_d8,_d9,_da,function(_dd,_de,_df){return new F(function(){return A(_db,[_dd,_de,new T(function(){var _dg=E(_df),_dh=E(_dg[2]);if(!_dh[0]){var _di=E(_dg);}else{var _dj=B(_cK(_dg[1],_dh,_d7)),_di=[0,E(_dj[1]),_dj[2]];}var _dk=_di;return _dk;})]);});},function(_dl){return new F(function(){return A(_dc,[new T(function(){return B(_d0(_dl,_d7));})]);});}]);});},_dm=new T(function(){return B(unCStr("space"));}),_dn=[1,_dm,_E],_do=[1,_e,_E],_dp=[0,E(_E)],_dq=[1,_dp,_E],_dr=function(_ds,_dt){var _du=_ds%_dt;if(_ds<=0){if(_ds>=0){return E(_du);}else{if(_dt<=0){return E(_du);}else{var _dv=E(_du);return _dv==0?0:_dv+_dt|0;}}}else{if(_dt>=0){if(_ds>=0){return E(_du);}else{if(_dt<=0){return E(_du);}else{var _dw=E(_du);return _dw==0?0:_dw+_dt|0;}}}else{var _dx=E(_du);return _dx==0?0:_dx+_dt|0;}}},_dy=function(_dz,_dA,_dB,_dC,_dD,_dE,_dF,_dG,_dH,_dI){var _dJ=[0,_dD,E(_dE),E(_dF)];return new F(function(){return A(_2W,[_dz,new T(function(){return B(A(_dA,[_dC]));}),function(_dK){var _dL=E(_dK);if(!_dL[0]){return E(new T(function(){return B(A(_dI,[[0,E(_dJ),_dq]]));}));}else{var _dM=E(_dL[1]),_dN=_dM[1],_dO=_dM[2];if(!B(A(_dB,[_dN]))){return new F(function(){return A(_dI,[[0,E(_dJ),[1,[0,E([1,_e,new T(function(){return B(_2M([1,_dN,_E],_do));})])],_E]]]);});}else{var _dP=E(_dE),_dQ=E(_dN);switch(E(_dQ[1])){case 9:var _dR=E(_dF)[1],_dS=[0,_dD,E(_dP),E([0,(_dR+8|0)-B(_dr(_dR-1|0,8))|0])];break;case 10:var _dS=[0,_dD,E([0,_dP[1]+1|0]),E(_2Y)];break;default:var _dS=[0,_dD,E(_dP),E([0,E(_dF)[1]+1|0])];}var _dT=_dS,_dU=[0,E(_dT),_E],_dV=[0,_dO,E(_dT),E(_dG)];return new F(function(){return A(_dH,[_dQ,_dV,_dU]);});}}}]);});},_dW=function(_dX){var _dY=E(_dX);switch(_dY){case 9:return true;case 10:return true;case 11:return true;case 12:return true;case 13:return true;case 32:return true;case 160:return true;default:var _dZ=u_iswspace(_dY),_e0=_dZ;return E(_e0)==0?false:true;}},_e1=function(_e2){return new F(function(){return _dW(E(_e2)[1]);});},_e3=[0],_e4=function(_e5,_e6){var _e7=E(_e6);return _e7[0]==0?B(A(_2Z,[_e5,_e3])):B(A(_2Z,[_e5,[1,[0,_e7[1],_e7[2]]]]));},_e8=function(_e9){return new F(function(){return _e4(_d,_e9);});},_ea=function(_eb,_ec,_ed,_ee,_ef){var _eg=E(_eb),_eh=E(_eg[2]);return new F(function(){return _dy(_d,_e8,_e1,_eg[1],_eh[1],_eh[2],_eh[3],_eg[3],_ec,_ef);});},_ei=function(_ej,_ek,_el,_em,_en){return new F(function(){return _d5(_ea,_dn,_ej,_ek,_el,_em,_en);});},_eo=function(_ep,_eq,_er,_es,_et){return new F(function(){return _aJ(_ei,_eq,function(_eu,_ev,_ew){return new F(function(){return A(_er,[_ep,_ev,new T(function(){var _ex=E(_ew),_ey=B(_am(_ex[1],_ex[2],E(_ev)[2],_E));return [0,E(_ey[1]),_ey[2]];})]);});},_es,function(_ez,_eA,_eB){return new F(function(){return A(_et,[_ep,_eA,new T(function(){var _eC=E(_eB),_eD=B(_am(_eC[1],_eC[2],E(_eA)[2],_E));return [0,E(_eD[1]),_eD[2]];})]);});});});},_eE=function(_eF){return [0,_eF,function(_cz){return new F(function(){return _e4(_eF,_cz);});}];},_eG=new T(function(){return B(_eE(_d));}),_eH=function(_eI,_eJ){return E(_eI)[1]==E(_eJ)[1];},_eK=function(_eL,_eM){return function(_eN,_eO,_eP,_eQ,_eR){return new F(function(){return _d5(function(_eS,_eT,_eU,_eV,_eW){var _eX=E(_eL),_eY=E(_eS),_eZ=E(_eY[2]);return new F(function(){return _dy(_eX[1],_eX[2],function(_f0){return new F(function(){return _eH(_f0,_eM);});},_eY[1],_eZ[1],_eZ[2],_eZ[3],_eY[3],_eT,_eW);});},[1,[1,_e,new T(function(){return B(_2M([1,_eM,_E],_do));})],_E],_eN,_eO,_eP,_eQ,_eR);});};},_f1=[0,45],_f2=new T(function(){return B(_eK(_eG,_f1));}),_f3=[0,39],_f4=new T(function(){return B(_eK(_eG,_f3));}),_f5=[0,34],_f6=new T(function(){return B(_eK(_eG,_f5));}),_f7=function(_f8,_f9,_fa,_fb,_fc,_fd){var _fe=[1,_f8];return new F(function(){return A(_f6,[_f9,function(_ff,_fg,_fh){return new F(function(){return A(_fa,[_fe,_fg,new T(function(){var _fi=E(_fh),_fj=B(_am(_fi[1],_fi[2],E(_fg)[2],_E));return [0,E(_fj[1]),_fj[2]];})]);});},_fb,function(_fk,_fl,_fm){return new F(function(){return A(_fc,[_fe,_fl,new T(function(){var _fn=E(_fm),_fo=B(_am(_fn[1],_fn[2],E(_fl)[2],_E));return [0,E(_fo[1]),_fo[2]];})]);});},_fd]);});},_fp=function(_fq,_fr,_fs,_ft,_fu,_fv,_fw){return new F(function(){return A(_fq,[_fs,_ft,_fu,_fv,function(_fx){return new F(function(){return A(_fr,[_fs,_ft,_fu,function(_fy,_fz,_fA){return new F(function(){return A(_fv,[_fy,_fz,new T(function(){return B(_bh(_fx,_fA));})]);});},function(_fB){return new F(function(){return A(_fw,[new T(function(){return B(_bh(_fx,_fB));})]);});}]);});}]);});},_fC=function(_fD,_fE){return E(_fD)[1]!=E(_fE)[1];},_fF=[0,_eH,_fC],_fG=new T(function(){return B(unCStr("\\\"nrt"));}),_fH=function(_fI){return E(E(_fI)[1]);},_fJ=function(_fK,_fL,_fM){while(1){var _fN=E(_fM);if(!_fN[0]){return false;}else{if(!B(A(_fH,[_fK,_fL,_fN[1]]))){_fM=_fN[2];continue;}else{return true;}}}},_fO=[0,92],_fP=[0,10],_fQ=[0,13],_fR=[0,9],_fS=function(_fT,_fU,_fV,_fW,_fX,_fY,_fZ){var _g0=[0,_fV],_g1=E(_fT);if(!_g1[0]){return new F(function(){return A(_fZ,[[0,E([0,_fU,E(_g0),E(_fW)]),_dq]]);});}else{var _g2=_g1[1],_g3=_g1[2];if(!B(_fJ(_fF,_g2,_fG))){return new F(function(){return A(_fZ,[[0,E([0,_fU,E(_g0),E(_fW)]),[1,[0,E([1,_e,new T(function(){return B(_2M([1,_g2,_E],_do));})])],_E]]]);});}else{var _g4=E(_g2)[1],_g5=function(_g6,_g7,_g8,_g9,_ga){return new F(function(){return A(_fY,[new T(function(){var _gb=E(_g4);switch(_gb){case 34:var _gc=E(_f5);break;case 92:var _gc=E(_fO);break;case 110:var _gc=E(_fP);break;case 114:var _gc=E(_fQ);break;case 116:var _gc=E(_fR);break;default:var _gc=[0,_gb];}return _gc;}),[0,_g6,E(_g7),E(_g8)],new T(function(){var _gd=B(_am(_g9,_ga,_g7,_E));return [0,E(_gd[1]),_gd[2]];})]);});};switch(E(_g4)){case 9:var _ge=E(_fW)[1],_gf=[0,_fU,E(_g0),E([0,(_ge+8|0)-B(_dr(_ge-1|0,8))|0])];return new F(function(){return _g5(_g3,_gf,_fX,_gf,_E);});break;case 10:var _gg=[0,_fU,E([0,_fV+1|0]),E(_2Y)];return new F(function(){return _g5(_g3,_gg,_fX,_gg,_E);});break;default:var _gh=[0,_fU,E(_g0),E([0,E(_fW)[1]+1|0])];return new F(function(){return _g5(_g3,_gh,_fX,_gh,_E);});}}}},_gi=new T(function(){return B(_eK(_eG,_fO));}),_gj=function(_gk,_gl,_gm,_gn){return new F(function(){return A(_gi,[_gk,function(_go,_gp,_gq){var _gr=E(_gp),_gs=E(_gr[2]);return new F(function(){return _fS(_gr[1],_gs[1],E(_gs[2])[1],_gs[3],_gr[3],_gl,function(_gt){return new F(function(){return A(_gm,[new T(function(){return B(_bh(_gq,_gt));})]);});});});},_gm,function(_gu,_gv,_gw){var _gx=E(_gv),_gy=E(_gx[2]);return new F(function(){return _fS(_gx[1],_gy[1],E(_gy[2])[1],_gy[3],_gx[3],_gl,function(_gz){return new F(function(){return A(_gn,[new T(function(){return B(_bh(_gw,_gz));})]);});});});},_gn]);});},_gA=function(_gB,_gC,_gD,_gE,_gF){return new F(function(){return _gj(_gB,_gC,_gD,_gF);});},_gG=function(_gH,_gI,_gJ,_gK,_e9){return new F(function(){return _gA(_gH,_gI,_gJ,_gK,_e9);});},_gL=function(_gM,_gN,_gO,_gP,_gQ,_gR,_gS,_gT,_gU,_gV){return new F(function(){return _dy(_gM,_gN,function(_gW){return !B(_fJ(_fF,_gW,_gO))?true:false;},_gP,_gQ,_gR,_gS,_gT,_gU,_gV);});},_gX=new T(function(){return B(unCStr("\"\\"));}),_gY=function(_gZ,_h0,_h1,_h2,_h3){var _h4=E(_gZ),_h5=E(_h4[2]);return new F(function(){return _gL(_d,_e8,_gX,_h4[1],_h5[1],_h5[2],_h5[3],_h4[3],_h0,_h3);});},_h6=function(_gH,_gI,_gJ,_gK,_e9){return new F(function(){return _fp(_gG,_gY,_gH,_gI,_gJ,_gK,_e9);});},_h7=function(_h8,_h9,_ha,_hb,_hc){return new F(function(){return _bn(_h6,_h8,function(_hd,_he,_hf){return new F(function(){return _f7(_hd,_he,_h9,_ha,function(_hg,_hh,_hi){return new F(function(){return A(_h9,[_hg,_hh,new T(function(){return B(_bh(_hf,_hi));})]);});},function(_hj){return new F(function(){return A(_ha,[new T(function(){return B(_bh(_hf,_hj));})]);});});});},_ha,function(_hk,_hl,_hm){return new F(function(){return _f7(_hk,_hl,_h9,_ha,function(_hn,_ho,_hp){return new F(function(){return A(_hb,[_hn,_ho,new T(function(){return B(_bh(_hm,_hp));})]);});},function(_hq){return new F(function(){return A(_hc,[new T(function(){return B(_bh(_hm,_hq));})]);});});});},_hc);});},_hr=true,_hs=[4,_hr],_ht=new T(function(){return B(unCStr("true"));}),_hu=false,_hv=[4,_hu],_hw=new T(function(){return B(unCStr("false"));}),_hx=function(_hy){return function(_hz,_hA,_hB,_hC,_hD){return new F(function(){return A(_hC,[new T(function(){return !B(_88(_hy,_hw))?!B(_88(_hy,_ht))?[2,_hy]:E(_hs):E(_hv);}),_hz,new T(function(){return [0,E(E(_hz)[2]),_E];})]);});};},_hE=new T(function(){return B(_eK(_eG,_f5));}),_hF=[0,40],_hG=new T(function(){return B(_eK(_eG,_hF));}),_hH=new T(function(){return B(unCStr("quote"));}),_hI=[2,_hH],_hJ=function(_hK,_hL,_hM,_hN,_hO){return new F(function(){return _hP(_hK,function(_hQ,_hR,_hS){return new F(function(){return A(_hL,[[0,_hI,_hQ],_hR,new T(function(){var _hT=E(_hS),_hU=B(_am(_hT[1],_hT[2],E(_hR)[2],_E));return [0,E(_hU[1]),_hU[2]];})]);});},_hM,function(_hV,_hW,_hX){return new F(function(){return A(_hN,[[0,_hI,_hV],_hW,new T(function(){var _hY=E(_hX),_hZ=B(_am(_hY[1],_hY[2],E(_hW)[2],_E));return [0,E(_hZ[1]),_hZ[2]];})]);});},_hO);});},_i0=function(_i1,_i2,_i3,_i4,_i5,_i6,_i7){var _i8=function(_i9,_ia,_ib,_ic,_id,_ie){return new F(function(){return _bn(function(_if,_ig,_ih,_ii,_ij){return new F(function(){return A(_i2,[_if,function(_ik,_il,_im){return new F(function(){return A(_i1,[_il,_ig,_ih,function(_in,_io,_ip){return new F(function(){return A(_ig,[_in,_io,new T(function(){return B(_bh(_im,_ip));})]);});},function(_iq){return new F(function(){return A(_ih,[new T(function(){return B(_bh(_im,_iq));})]);});}]);});},_ih,function(_ir,_is,_it){return new F(function(){return A(_i1,[_is,_ig,_ih,function(_iu,_iv,_iw){return new F(function(){return A(_ii,[_iu,_iv,new T(function(){return B(_bh(_it,_iw));})]);});},function(_ix){return new F(function(){return A(_ij,[new T(function(){return B(_bh(_it,_ix));})]);});}]);});},_ij]);});},_ia,function(_iy,_iz,_iA){return new F(function(){return A(_ib,[[1,_i9,_iy],_iz,new T(function(){var _iB=E(_iA),_iC=B(_am(_iB[1],_iB[2],E(_iz)[2],_E));return [0,E(_iC[1]),_iC[2]];})]);});},_ic,function(_iD,_iE,_iF){return new F(function(){return A(_id,[[1,_i9,_iD],_iE,new T(function(){var _iG=E(_iF),_iH=B(_am(_iG[1],_iG[2],E(_iE)[2],_E));return [0,E(_iH[1]),_iH[2]];})]);});},_ie);});};return new F(function(){return A(_i1,[_i3,function(_iI,_iJ,_iK){return new F(function(){return _i8(_iI,_iJ,_i4,_i5,function(_iL,_iM,_iN){return new F(function(){return A(_i4,[_iL,_iM,new T(function(){return B(_bh(_iK,_iN));})]);});},function(_iO){return new F(function(){return A(_i5,[new T(function(){return B(_bh(_iK,_iO));})]);});});});},_i5,function(_iP,_iQ,_iR){return new F(function(){return _i8(_iP,_iQ,_i4,_i5,function(_iS,_iT,_iU){return new F(function(){return A(_i6,[_iS,_iT,new T(function(){return B(_bh(_iR,_iU));})]);});},function(_iV){return new F(function(){return A(_i7,[new T(function(){return B(_bh(_iR,_iV));})]);});});});},_i7]);});},_iW=function(_iX,_iY,_iZ,_j0,_j1){return new F(function(){return _aJ(_ei,_iX,_iY,_iZ,_j0);});},_j2=[0,41],_j3=new T(function(){return B(_eK(_eG,_j2));}),_j4=function(_j5){var _j6=E(_j5);return _j6[0]==0?[6]:[0,_j6[1],new T(function(){return B(_j4(_j6[2]));})];},_j7=function(_j8){var _j9=new T(function(){return B(_j4(_j8));}),_ja=function(_jb,_jc,_jd,_je,_jf){return new F(function(){return A(_j3,[_jb,function(_jg,_jh,_ji){return new F(function(){return A(_jc,[_j9,_jh,new T(function(){var _jj=E(_ji),_jk=B(_am(_jj[1],_jj[2],E(_jh)[2],_E));return [0,E(_jk[1]),_jk[2]];})]);});},_jd,function(_jl,_jm,_jn){return new F(function(){return A(_je,[_j9,_jm,new T(function(){var _jo=E(_jn),_jp=B(_am(_jo[1],_jo[2],E(_jm)[2],_E));return [0,E(_jp[1]),_jp[2]];})]);});},_jf]);});};return function(_jq,_jr,_js,_jt,_ju){return new F(function(){return _aJ(_ei,_jq,function(_jv,_jw,_jx){return new F(function(){return _ja(_jw,_jr,_js,function(_jy,_jz,_jA){return new F(function(){return A(_jr,[_jy,_jz,new T(function(){return B(_bh(_jx,_jA));})]);});},function(_jB){return new F(function(){return A(_js,[new T(function(){return B(_bh(_jx,_jB));})]);});});});},_js,function(_jC,_jD,_jE){return new F(function(){return _ja(_jD,_jr,_js,function(_jF,_jG,_jH){return new F(function(){return A(_jt,[_jF,_jG,new T(function(){return B(_bh(_jE,_jH));})]);});},function(_jI){return new F(function(){return A(_ju,[new T(function(){return B(_bh(_jE,_jI));})]);});});});});});};},_jJ=function(_jK,_jL,_jM,_jN,_jO){var _jP=function(_jQ,_jR,_jS){return new F(function(){return A(_j7,[_jQ,_jR,_jL,_jM,function(_jT,_jU,_jV){return new F(function(){return A(_jN,[_jT,_jU,new T(function(){return B(_bh(_jS,_jV));})]);});},function(_jW){return new F(function(){return A(_jO,[new T(function(){return B(_bh(_jS,_jW));})]);});}]);});};return new F(function(){return _i0(_hP,_iW,_jK,function(_jX,_jY,_jZ){return new F(function(){return A(_j7,[_jX,_jY,_jL,_jM,function(_k0,_k1,_k2){return new F(function(){return A(_jL,[_k0,_k1,new T(function(){return B(_bh(_jZ,_k2));})]);});},function(_k3){return new F(function(){return A(_jM,[new T(function(){return B(_bh(_jZ,_k3));})]);});}]);});},_jM,_jP,function(_k4){return new F(function(){return _jP(_E,_jK,new T(function(){var _k5=E(_k4),_k6=B(_am(_k5[1],_k5[2],E(_jK)[2],_E));return [0,E(_k6[1]),_k6[2]];}));});});});},_k7=function(_k8,_k9,_ka,_kb,_kc){return new F(function(){return _aJ(_ei,_k8,function(_kd,_ke,_kf){return new F(function(){return _jJ(_ke,_k9,_ka,function(_kg,_kh,_ki){return new F(function(){return A(_k9,[_kg,_kh,new T(function(){return B(_bh(_kf,_ki));})]);});},function(_kj){return new F(function(){return A(_ka,[new T(function(){return B(_bh(_kf,_kj));})]);});});});},_ka,function(_kk,_kl,_km){return new F(function(){return _jJ(_kl,_k9,_ka,function(_kn,_ko,_kp){return new F(function(){return A(_kb,[_kn,_ko,new T(function(){return B(_bh(_km,_kp));})]);});},function(_kq){return new F(function(){return A(_kc,[new T(function(){return B(_bh(_km,_kq));})]);});});});});});},_kr=new T(function(){return B(unCStr(")\n\r	 "));}),_ks=function(_kt,_ku,_kv,_kw,_kx){var _ky=E(_kt),_kz=E(_ky[2]);return new F(function(){return _gL(_d,_e8,_kr,_ky[1],_kz[1],_kz[2],_kz[3],_ky[3],_ku,_kx);});},_kA=function(_kB){var _kC=E(_kB)[1];return _kC<48?false:_kC<=57;},_kD=function(_kE,_kF,_kG,_kH,_kI){var _kJ=E(_kE),_kK=E(_kJ[2]);return new F(function(){return _dy(_d,_e8,_kA,_kJ[1],_kK[1],_kK[2],_kK[3],_kJ[3],_kF,_kI);});},_kL=new T(function(){return B(unCStr("digit"));}),_kM=[1,_kL,_E],_kN=function(_kO,_kP,_kQ,_kR,_kS){return new F(function(){return _d5(_kD,_kM,_kO,_kP,_kQ,_kR,_kS);});},_kT=function(_kU,_kV){while(1){var _kW=E(_kV);if(!_kW[0]){return true;}else{if(!B(A(_kU,[_kW[1]]))){return false;}else{_kV=_kW[2];continue;}}}},_kX=function(_kY,_kZ){var _l0=jsShowI(_kY),_l1=_l0;return new F(function(){return _f(fromJSStr(_l1),_kZ);});},_l2=function(_l3,_l4,_l5){return _l4>=0?B(_kX(_l4,_l5)):_l3<=6?B(_kX(_l4,_l5)):[1,_y,new T(function(){var _l6=jsShowI(_l4),_l7=_l6;return B(_f(fromJSStr(_l7),[1,_x,_l5]));})];},_l8=[0,41],_l9=[1,_l8,_E],_la=new T(function(){return B(_l2(0,29,_l9));}),_lb=new T(function(){return B(unAppCStr(") is outside of enumeration\'s range (0,",_la));}),_lc=function(_ld){return new F(function(){return err(B(unAppCStr("toEnum{GeneralCategory}: tag (",new T(function(){return B(_l2(0,_ld,_lb));}))));});},_le=function(_lf){var _lg=u_gencat(_lf),_lh=_lg;return _lh<0?B(_lc(_lh)):_lh>29?B(_lc(_lh)):_lh;},_li=function(_lj){switch(B(_le(E(_lj)[1]))){case 8:return true;case 9:return true;case 10:return true;default:return false;}},_lk=[1,_f1,_E],_ll=[2,_lk],_lm=new T(function(){return B(unCStr("Prelude.read: no parse"));}),_ln=new T(function(){return B(err(_lm));}),_lo=new T(function(){return B(unCStr("Prelude.read: ambiguous parse"));}),_lp=new T(function(){return B(err(_lo));}),_lq=new T(function(){return B(unCStr("Control.Exception.Base"));}),_lr=new T(function(){return B(unCStr("base"));}),_ls=new T(function(){return B(unCStr("PatternMatchFail"));}),_lt=new T(function(){var _lu=hs_wordToWord64(18445595),_lv=_lu,_lw=hs_wordToWord64(52003073),_lx=_lw;return [0,_lv,_lx,[0,_lv,_lx,_lr,_lq,_ls],_E];}),_ly=function(_lz){return E(_lt);},_lA=function(_lB){return E(E(_lB)[1]);},_lC=new T(function(){return B(unCStr("Maybe.fromJust: Nothing"));}),_lD=new T(function(){return B(err(_lC));}),_lE=function(_lF,_lG,_lH){var _lI=new T(function(){var _lJ=B(A(_lF,[_lH])),_lK=B(A(_lG,[new T(function(){var _lL=E(_lI);return _lL[0]==0?E(_lD):E(_lL[1]);})])),_lM=hs_eqWord64(_lJ[1],_lK[1]),_lN=_lM;if(!E(_lN)){var _lO=[0];}else{var _lP=hs_eqWord64(_lJ[2],_lK[2]),_lQ=_lP,_lO=E(_lQ)==0?[0]:[1,_lH];}var _lR=_lO,_lS=_lR;return _lS;});return E(_lI);},_lT=function(_lU){var _lV=E(_lU);return new F(function(){return _lE(B(_lA(_lV[1])),_ly,_lV[2]);});},_lW=function(_lX){return E(E(_lX)[1]);},_lY=function(_lZ,_m0){return new F(function(){return _f(E(_lZ)[1],_m0);});},_m1=function(_m2,_m3){return new F(function(){return _5M(_lY,_m2,_m3);});},_m4=function(_m5,_m6,_m7){return new F(function(){return _f(E(_m6)[1],_m7);});},_m8=[0,_m4,_lW,_m1],_m9=new T(function(){return [0,_ly,_m8,_ma,_lT];}),_ma=function(_mb){return [0,_m9,_mb];},_mc=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_md=function(_me,_mf){return new F(function(){return die(new T(function(){return B(A(_mf,[_me]));}));});},_mg=function(_mh,_mi){var _mj=E(_mi);if(!_mj[0]){return [0,_E,_E];}else{var _mk=_mj[1];if(!B(A(_mh,[_mk]))){return [0,_E,_mj];}else{var _ml=new T(function(){var _mm=B(_mg(_mh,_mj[2]));return [0,_mm[1],_mm[2]];});return [0,[1,_mk,new T(function(){return E(E(_ml)[1]);})],new T(function(){return E(E(_ml)[2]);})];}}},_mn=[0,32],_mo=[0,10],_mp=[1,_mo,_E],_mq=function(_mr){return E(E(_mr)[1])==124?false:true;},_ms=function(_mt,_mu){var _mv=B(_mg(_mq,B(unCStr(_mt)))),_mw=_mv[1],_mx=function(_my,_mz){return new F(function(){return _f(_my,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_f(_mu,new T(function(){return B(_f(_mz,_mp));})));})));}));});},_mA=E(_mv[2]);if(!_mA[0]){return new F(function(){return _mx(_mw,_E);});}else{return E(E(_mA[1])[1])==124?B(_mx(_mw,[1,_mn,_mA[2]])):B(_mx(_mw,_E));}},_mB=function(_mC){return new F(function(){return _md([0,new T(function(){return B(_ms(_mC,_mc));})],_ma);});},_mD=new T(function(){return B(_mB("Text/ParserCombinators/ReadP.hs:(134,3)-(157,60)|function mplus"));}),_mE=function(_mF,_mG){while(1){var _mH=(function(_mI,_mJ){var _mK=E(_mI);switch(_mK[0]){case 0:var _mL=E(_mJ);if(!_mL[0]){return [0];}else{_mF=B(A(_mK[1],[_mL[1]]));_mG=_mL[2];return null;}break;case 1:var _mM=B(A(_mK[1],[_mJ])),_mN=_mJ;_mF=_mM;_mG=_mN;return null;case 2:return [0];case 3:return [1,[0,_mK[1],_mJ],new T(function(){return B(_mE(_mK[2],_mJ));})];default:return E(_mK[1]);}})(_mF,_mG);if(_mH!=null){return _mH;}}},_mO=function(_mP,_mQ){var _mR=new T(function(){var _mS=E(_mQ);if(_mS[0]==3){var _mT=[3,_mS[1],new T(function(){return B(_mO(_mP,_mS[2]));})];}else{var _mU=E(_mP);if(_mU[0]==2){var _mV=E(_mS);}else{var _mW=E(_mS);if(_mW[0]==2){var _mX=E(_mU);}else{var _mY=new T(function(){var _mZ=E(_mW);if(_mZ[0]==4){var _n0=[1,function(_n1){return [4,new T(function(){return B(_f(B(_mE(_mU,_n1)),_mZ[1]));})];}];}else{var _n2=E(_mU);if(_n2[0]==1){var _n3=_n2[1],_n4=E(_mZ);if(!_n4[0]){var _n5=[1,function(_n6){return new F(function(){return _mO(B(A(_n3,[_n6])),_n4);});}];}else{var _n5=[1,function(_n7){return new F(function(){return _mO(B(A(_n3,[_n7])),new T(function(){return B(A(_n4[1],[_n7]));}));});}];}var _n8=_n5;}else{var _n9=E(_mZ);if(!_n9[0]){var _na=E(_mD);}else{var _na=[1,function(_nb){return new F(function(){return _mO(_n2,new T(function(){return B(A(_n9[1],[_nb]));}));});}];}var _n8=_na;}var _n0=_n8;}return _n0;}),_nc=E(_mU);switch(_nc[0]){case 1:var _nd=E(_mW);if(_nd[0]==4){var _ne=[1,function(_nf){return [4,new T(function(){return B(_f(B(_mE(B(A(_nc[1],[_nf])),_nf)),_nd[1]));})];}];}else{var _ne=E(_mY);}var _ng=_ne;break;case 4:var _nh=_nc[1],_ni=E(_mW);switch(_ni[0]){case 0:var _nj=[1,function(_nk){return [4,new T(function(){return B(_f(_nh,new T(function(){return B(_mE(_ni,_nk));})));})];}];break;case 1:var _nj=[1,function(_nl){return [4,new T(function(){return B(_f(_nh,new T(function(){return B(_mE(B(A(_ni[1],[_nl])),_nl));})));})];}];break;default:var _nj=[4,new T(function(){return B(_f(_nh,_ni[1]));})];}var _ng=_nj;break;default:var _ng=E(_mY);}var _mX=_ng;}var _mV=_mX;}var _mT=_mV;}return _mT;}),_nm=E(_mP);switch(_nm[0]){case 0:var _nn=E(_mQ);return _nn[0]==0?[0,function(_no){return new F(function(){return _mO(B(A(_nm[1],[_no])),new T(function(){return B(A(_nn[1],[_no]));}));});}]:E(_mR);case 3:return [3,_nm[1],new T(function(){return B(_mO(_nm[2],_mQ));})];default:return E(_mR);}},_np=function(_nq,_nr,_ns){while(1){var _nt=E(_nr);if(!_nt[0]){return E(_ns)[0]==0?true:false;}else{var _nu=E(_ns);if(!_nu[0]){return false;}else{if(!B(A(_fH,[_nq,_nt[1],_nu[1]]))){return false;}else{_nr=_nt[2];_ns=_nu[2];continue;}}}}},_nv=function(_nw,_nx,_ny){return !B(_np(_nw,_nx,_ny))?true:false;},_nz=function(_nA){return [0,function(_nB,_nC){return new F(function(){return _np(_nA,_nB,_nC);});},function(_nB,_nC){return new F(function(){return _nv(_nA,_nB,_nC);});}];},_nD=new T(function(){return B(_nz(_fF));}),_nE=function(_nF,_nG){var _nH=E(_nF);switch(_nH[0]){case 0:return [0,function(_nI){return new F(function(){return _nE(B(A(_nH[1],[_nI])),_nG);});}];case 1:return [1,function(_nJ){return new F(function(){return _nE(B(A(_nH[1],[_nJ])),_nG);});}];case 2:return [2];case 3:return new F(function(){return _mO(B(A(_nG,[_nH[1]])),new T(function(){return B(_nE(_nH[2],_nG));}));});break;default:var _nK=function(_nL){var _nM=E(_nL);if(!_nM[0]){return [0];}else{var _nN=E(_nM[1]);return new F(function(){return _f(B(_mE(B(A(_nG,[_nN[1]])),_nN[2])),new T(function(){return B(_nK(_nM[2]));}));});}},_nO=B(_nK(_nH[1]));return _nO[0]==0?[2]:[4,_nO];}},_nP=[2],_nQ=function(_nR){return [3,_nR,_nP];},_nS=function(_nT,_nU){var _nV=E(_nT);if(!_nV){return new F(function(){return A(_nU,[_3u]);});}else{return [0,function(_nW){return E(new T(function(){return B(_nS(_nV-1|0,_nU));}));}];}},_nX=function(_nY,_nZ,_o0){return [1,function(_o1){return new F(function(){return A(function(_o2,_o3,_o4){while(1){var _o5=(function(_o6,_o7,_o8){var _o9=E(_o6);switch(_o9[0]){case 0:var _oa=E(_o7);if(!_oa[0]){return E(_nZ);}else{_o2=B(A(_o9[1],[_oa[1]]));_o3=_oa[2];var _ob=_o8+1|0;_o4=_ob;return null;}break;case 1:var _oc=B(A(_o9[1],[_o7])),_od=_o7,_ob=_o8;_o2=_oc;_o3=_od;_o4=_ob;return null;case 2:return E(_nZ);case 3:return function(_oe){return new F(function(){return _nS(_o8,function(_of){return E(new T(function(){return B(_nE(_o9,_oe));}));});});};default:return function(_og){return new F(function(){return _nE(_o9,_og);});};}})(_o2,_o3,_o4);if(_o5!=null){return _o5;}}},[new T(function(){return B(A(_nY,[_nQ]));}),_o1,0,_o0]);});}];},_oh=[6],_oi=new T(function(){return B(unCStr("valDig: Bad base"));}),_oj=new T(function(){return B(err(_oi));}),_ok=function(_ol,_om){var _on=function(_oo,_op){var _oq=E(_oo);if(!_oq[0]){return function(_or){return new F(function(){return A(_or,[new T(function(){return B(A(_op,[_E]));})]);});};}else{var _os=E(_oq[1])[1],_ot=function(_ou){return function(_ov){return [0,function(_ow){return E(new T(function(){return B(A(new T(function(){return B(_on(_oq[2],function(_ox){return new F(function(){return A(_op,[[1,_ou,_ox]]);});}));}),[_ov]));}));}];};};switch(E(E(_ol)[1])){case 8:if(48>_os){return function(_oy){return new F(function(){return A(_oy,[new T(function(){return B(A(_op,[_E]));})]);});};}else{if(_os>55){return function(_oz){return new F(function(){return A(_oz,[new T(function(){return B(A(_op,[_E]));})]);});};}else{return new F(function(){return _ot([0,_os-48|0]);});}}break;case 10:if(48>_os){return function(_oA){return new F(function(){return A(_oA,[new T(function(){return B(A(_op,[_E]));})]);});};}else{if(_os>57){return function(_oB){return new F(function(){return A(_oB,[new T(function(){return B(A(_op,[_E]));})]);});};}else{return new F(function(){return _ot([0,_os-48|0]);});}}break;case 16:var _oC=new T(function(){if(97>_os){if(65>_os){var _oD=[0];}else{if(_os>70){var _oE=[0];}else{var _oE=[1,[0,(_os-65|0)+10|0]];}var _oD=_oE;}var _oF=_oD;}else{if(_os>102){if(65>_os){var _oG=[0];}else{if(_os>70){var _oH=[0];}else{var _oH=[1,[0,(_os-65|0)+10|0]];}var _oG=_oH;}var _oI=_oG;}else{var _oI=[1,[0,(_os-97|0)+10|0]];}var _oF=_oI;}return _oF;});if(48>_os){var _oJ=E(_oC);if(!_oJ[0]){return function(_oK){return new F(function(){return A(_oK,[new T(function(){return B(A(_op,[_E]));})]);});};}else{return new F(function(){return _ot(_oJ[1]);});}}else{if(_os>57){var _oL=E(_oC);if(!_oL[0]){return function(_oM){return new F(function(){return A(_oM,[new T(function(){return B(A(_op,[_E]));})]);});};}else{return new F(function(){return _ot(_oL[1]);});}}else{return new F(function(){return _ot([0,_os-48|0]);});}}break;default:return E(_oj);}}};return [1,function(_oN){return new F(function(){return A(_on,[_oN,_a2,function(_oO){var _oP=E(_oO);return _oP[0]==0?[2]:B(A(_om,[_oP]));}]);});}];},_oQ=[0,10],_oR=[0,1],_oS=[0,2147483647],_oT=function(_oU,_oV){while(1){var _oW=E(_oU);if(!_oW[0]){var _oX=_oW[1],_oY=E(_oV);if(!_oY[0]){var _oZ=_oY[1],_p0=addC(_oX,_oZ);if(!E(_p0[2])){return [0,_p0[1]];}else{_oU=[1,I_fromInt(_oX)];_oV=[1,I_fromInt(_oZ)];continue;}}else{_oU=[1,I_fromInt(_oX)];_oV=_oY;continue;}}else{var _p1=E(_oV);if(!_p1[0]){_oU=_oW;_oV=[1,I_fromInt(_p1[1])];continue;}else{return [1,I_add(_oW[1],_p1[1])];}}}},_p2=new T(function(){return B(_oT(_oS,_oR));}),_p3=function(_p4){var _p5=E(_p4);if(!_p5[0]){var _p6=E(_p5[1]);return _p6==(-2147483648)?E(_p2):[0, -_p6];}else{return [1,I_negate(_p5[1])];}},_p7=[0,10],_p8=[0,0],_p9=function(_pa){return [0,_pa];},_pb=function(_pc,_pd){while(1){var _pe=E(_pc);if(!_pe[0]){var _pf=_pe[1],_pg=E(_pd);if(!_pg[0]){var _ph=_pg[1];if(!(imul(_pf,_ph)|0)){return [0,imul(_pf,_ph)|0];}else{_pc=[1,I_fromInt(_pf)];_pd=[1,I_fromInt(_ph)];continue;}}else{_pc=[1,I_fromInt(_pf)];_pd=_pg;continue;}}else{var _pi=E(_pd);if(!_pi[0]){_pc=_pe;_pd=[1,I_fromInt(_pi[1])];continue;}else{return [1,I_mul(_pe[1],_pi[1])];}}}},_pj=function(_pk,_pl,_pm){while(1){var _pn=E(_pm);if(!_pn[0]){return E(_pl);}else{var _po=B(_oT(B(_pb(_pl,_pk)),B(_p9(E(_pn[1])[1]))));_pm=_pn[2];_pl=_po;continue;}}},_pp=function(_pq){var _pr=new T(function(){return B(_mO(B(_mO([0,function(_ps){if(E(E(_ps)[1])==45){return new F(function(){return _ok(_oQ,function(_pt){return new F(function(){return A(_pq,[[1,new T(function(){return B(_p3(B(_pj(_p7,_p8,_pt))));})]]);});});});}else{return [2];}}],[0,function(_pu){if(E(E(_pu)[1])==43){return new F(function(){return _ok(_oQ,function(_pv){return new F(function(){return A(_pq,[[1,new T(function(){return B(_pj(_p7,_p8,_pv));})]]);});});});}else{return [2];}}])),new T(function(){return B(_ok(_oQ,function(_pw){return new F(function(){return A(_pq,[[1,new T(function(){return B(_pj(_p7,_p8,_pw));})]]);});}));})));});return new F(function(){return _mO([0,function(_px){return E(E(_px)[1])==101?E(_pr):[2];}],[0,function(_py){return E(E(_py)[1])==69?E(_pr):[2];}]);});},_pz=function(_pA){return new F(function(){return A(_pA,[_e3]);});},_pB=function(_pC){return new F(function(){return A(_pC,[_e3]);});},_pD=function(_pE){return [0,function(_pF){return E(E(_pF)[1])==46?E(new T(function(){return B(_ok(_oQ,function(_pG){return new F(function(){return A(_pE,[[1,_pG]]);});}));})):[2];}];},_pH=function(_pI){return new F(function(){return _ok(_oQ,function(_pJ){return new F(function(){return _nX(_pD,_pz,function(_pK){return new F(function(){return _nX(_pp,_pB,function(_pL){return new F(function(){return A(_pI,[[5,[1,_pJ,_pK,_pL]]]);});});});});});});});},_pM=new T(function(){return B(unCStr("!@#$%&*+./<=>?\\^|:-~"));}),_pN=function(_pO){return new F(function(){return _fJ(_fF,_pO,_pM);});},_pP=[0,8],_pQ=[0,16],_pR=function(_pS){return [0,function(_pT){return E(E(_pT)[1])==48?E([0,function(_pU){switch(E(E(_pU)[1])){case 79:return E(new T(function(){return B(_ok(_pP,function(_pV){return new F(function(){return A(_pS,[[5,[0,_pP,_pV]]]);});}));}));case 88:return E(new T(function(){return B(_ok(_pQ,function(_pW){return new F(function(){return A(_pS,[[5,[0,_pQ,_pW]]]);});}));}));case 111:return E(new T(function(){return B(_ok(_pP,function(_pX){return new F(function(){return A(_pS,[[5,[0,_pP,_pX]]]);});}));}));case 120:return E(new T(function(){return B(_ok(_pQ,function(_pY){return new F(function(){return A(_pS,[[5,[0,_pQ,_pY]]]);});}));}));default:return [2];}}]):[2];}];},_pZ=function(_q0){return [0,function(_q1){switch(E(E(_q1)[1])){case 79:return E(new T(function(){return B(A(_q0,[_pP]));}));case 88:return E(new T(function(){return B(A(_q0,[_pQ]));}));case 111:return E(new T(function(){return B(A(_q0,[_pP]));}));case 120:return E(new T(function(){return B(A(_q0,[_pQ]));}));default:return [2];}}];},_q2=function(_q3){return new F(function(){return A(_q3,[_oQ]);});},_q4=function(_q5){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_l2(9,_q5,_E));}))));});},_q6=function(_q7){var _q8=E(_q7);return _q8[0]==0?E(_q8[1]):I_toInt(_q8[1]);},_q9=function(_qa,_qb){var _qc=E(_qa);if(!_qc[0]){var _qd=_qc[1],_qe=E(_qb);return _qe[0]==0?_qd<=_qe[1]:I_compareInt(_qe[1],_qd)>=0;}else{var _qf=_qc[1],_qg=E(_qb);return _qg[0]==0?I_compareInt(_qf,_qg[1])<=0:I_compare(_qf,_qg[1])<=0;}},_qh=function(_qi){return [2];},_qj=function(_qk){var _ql=E(_qk);if(!_ql[0]){return E(_qh);}else{var _qm=_ql[1],_qn=E(_ql[2]);return _qn[0]==0?E(_qm):function(_qo){return new F(function(){return _mO(B(A(_qm,[_qo])),new T(function(){return B(A(new T(function(){return B(_qj(_qn));}),[_qo]));}));});};}},_qp=new T(function(){return B(unCStr("NUL"));}),_qq=function(_qr){return [2];},_qs=function(_qt){return new F(function(){return _qq(_qt);});},_qu=function(_qv,_qw){var _qx=function(_qy,_qz){var _qA=E(_qy);if(!_qA[0]){return function(_qB){return new F(function(){return A(_qB,[_qv]);});};}else{var _qC=E(_qz);return _qC[0]==0?E(_qq):E(_qA[1])[1]!=E(_qC[1])[1]?E(_qs):function(_qD){return [0,function(_qE){return E(new T(function(){return B(A(new T(function(){return B(_qx(_qA[2],_qC[2]));}),[_qD]));}));}];};}};return [1,function(_qF){return new F(function(){return A(_qx,[_qv,_qF,_qw]);});}];},_qG=[0,0],_qH=function(_qI){return new F(function(){return _qu(_qp,function(_qJ){return E(new T(function(){return B(A(_qI,[_qG]));}));});});},_qK=new T(function(){return B(unCStr("STX"));}),_qL=[0,2],_qM=function(_qN){return new F(function(){return _qu(_qK,function(_qO){return E(new T(function(){return B(A(_qN,[_qL]));}));});});},_qP=new T(function(){return B(unCStr("ETX"));}),_qQ=[0,3],_qR=function(_qS){return new F(function(){return _qu(_qP,function(_qT){return E(new T(function(){return B(A(_qS,[_qQ]));}));});});},_qU=new T(function(){return B(unCStr("EOT"));}),_qV=[0,4],_qW=function(_qX){return new F(function(){return _qu(_qU,function(_qY){return E(new T(function(){return B(A(_qX,[_qV]));}));});});},_qZ=new T(function(){return B(unCStr("ENQ"));}),_r0=[0,5],_r1=function(_r2){return new F(function(){return _qu(_qZ,function(_r3){return E(new T(function(){return B(A(_r2,[_r0]));}));});});},_r4=new T(function(){return B(unCStr("ACK"));}),_r5=[0,6],_r6=function(_r7){return new F(function(){return _qu(_r4,function(_r8){return E(new T(function(){return B(A(_r7,[_r5]));}));});});},_r9=new T(function(){return B(unCStr("BEL"));}),_ra=[0,7],_rb=function(_rc){return new F(function(){return _qu(_r9,function(_rd){return E(new T(function(){return B(A(_rc,[_ra]));}));});});},_re=new T(function(){return B(unCStr("BS"));}),_rf=[0,8],_rg=function(_rh){return new F(function(){return _qu(_re,function(_ri){return E(new T(function(){return B(A(_rh,[_rf]));}));});});},_rj=new T(function(){return B(unCStr("HT"));}),_rk=[0,9],_rl=function(_rm){return new F(function(){return _qu(_rj,function(_rn){return E(new T(function(){return B(A(_rm,[_rk]));}));});});},_ro=new T(function(){return B(unCStr("LF"));}),_rp=[0,10],_rq=function(_rr){return new F(function(){return _qu(_ro,function(_rs){return E(new T(function(){return B(A(_rr,[_rp]));}));});});},_rt=new T(function(){return B(unCStr("VT"));}),_ru=[0,11],_rv=function(_rw){return new F(function(){return _qu(_rt,function(_rx){return E(new T(function(){return B(A(_rw,[_ru]));}));});});},_ry=new T(function(){return B(unCStr("FF"));}),_rz=[0,12],_rA=function(_rB){return new F(function(){return _qu(_ry,function(_rC){return E(new T(function(){return B(A(_rB,[_rz]));}));});});},_rD=new T(function(){return B(unCStr("CR"));}),_rE=[0,13],_rF=function(_rG){return new F(function(){return _qu(_rD,function(_rH){return E(new T(function(){return B(A(_rG,[_rE]));}));});});},_rI=new T(function(){return B(unCStr("SI"));}),_rJ=[0,15],_rK=function(_rL){return new F(function(){return _qu(_rI,function(_rM){return E(new T(function(){return B(A(_rL,[_rJ]));}));});});},_rN=new T(function(){return B(unCStr("DLE"));}),_rO=[0,16],_rP=function(_rQ){return new F(function(){return _qu(_rN,function(_rR){return E(new T(function(){return B(A(_rQ,[_rO]));}));});});},_rS=new T(function(){return B(unCStr("DC1"));}),_rT=[0,17],_rU=function(_rV){return new F(function(){return _qu(_rS,function(_rW){return E(new T(function(){return B(A(_rV,[_rT]));}));});});},_rX=new T(function(){return B(unCStr("DC2"));}),_rY=[0,18],_rZ=function(_s0){return new F(function(){return _qu(_rX,function(_s1){return E(new T(function(){return B(A(_s0,[_rY]));}));});});},_s2=new T(function(){return B(unCStr("DC3"));}),_s3=[0,19],_s4=function(_s5){return new F(function(){return _qu(_s2,function(_s6){return E(new T(function(){return B(A(_s5,[_s3]));}));});});},_s7=new T(function(){return B(unCStr("DC4"));}),_s8=[0,20],_s9=function(_sa){return new F(function(){return _qu(_s7,function(_sb){return E(new T(function(){return B(A(_sa,[_s8]));}));});});},_sc=new T(function(){return B(unCStr("NAK"));}),_sd=[0,21],_se=function(_sf){return new F(function(){return _qu(_sc,function(_sg){return E(new T(function(){return B(A(_sf,[_sd]));}));});});},_sh=new T(function(){return B(unCStr("SYN"));}),_si=[0,22],_sj=function(_sk){return new F(function(){return _qu(_sh,function(_sl){return E(new T(function(){return B(A(_sk,[_si]));}));});});},_sm=new T(function(){return B(unCStr("ETB"));}),_sn=[0,23],_so=function(_sp){return new F(function(){return _qu(_sm,function(_sq){return E(new T(function(){return B(A(_sp,[_sn]));}));});});},_sr=new T(function(){return B(unCStr("CAN"));}),_ss=[0,24],_st=function(_su){return new F(function(){return _qu(_sr,function(_sv){return E(new T(function(){return B(A(_su,[_ss]));}));});});},_sw=new T(function(){return B(unCStr("EM"));}),_sx=[0,25],_sy=function(_sz){return new F(function(){return _qu(_sw,function(_sA){return E(new T(function(){return B(A(_sz,[_sx]));}));});});},_sB=new T(function(){return B(unCStr("SUB"));}),_sC=[0,26],_sD=function(_sE){return new F(function(){return _qu(_sB,function(_sF){return E(new T(function(){return B(A(_sE,[_sC]));}));});});},_sG=new T(function(){return B(unCStr("ESC"));}),_sH=[0,27],_sI=function(_sJ){return new F(function(){return _qu(_sG,function(_sK){return E(new T(function(){return B(A(_sJ,[_sH]));}));});});},_sL=new T(function(){return B(unCStr("FS"));}),_sM=[0,28],_sN=function(_sO){return new F(function(){return _qu(_sL,function(_sP){return E(new T(function(){return B(A(_sO,[_sM]));}));});});},_sQ=new T(function(){return B(unCStr("GS"));}),_sR=[0,29],_sS=function(_sT){return new F(function(){return _qu(_sQ,function(_sU){return E(new T(function(){return B(A(_sT,[_sR]));}));});});},_sV=new T(function(){return B(unCStr("RS"));}),_sW=[0,30],_sX=function(_sY){return new F(function(){return _qu(_sV,function(_sZ){return E(new T(function(){return B(A(_sY,[_sW]));}));});});},_t0=new T(function(){return B(unCStr("US"));}),_t1=[0,31],_t2=function(_t3){return new F(function(){return _qu(_t0,function(_t4){return E(new T(function(){return B(A(_t3,[_t1]));}));});});},_t5=new T(function(){return B(unCStr("SP"));}),_t6=[0,32],_t7=function(_t8){return new F(function(){return _qu(_t5,function(_t9){return E(new T(function(){return B(A(_t8,[_t6]));}));});});},_ta=new T(function(){return B(unCStr("DEL"));}),_tb=[0,127],_tc=function(_td){return new F(function(){return _qu(_ta,function(_te){return E(new T(function(){return B(A(_td,[_tb]));}));});});},_tf=[1,_tc,_E],_tg=[1,_t7,_tf],_th=[1,_t2,_tg],_ti=[1,_sX,_th],_tj=[1,_sS,_ti],_tk=[1,_sN,_tj],_tl=[1,_sI,_tk],_tm=[1,_sD,_tl],_tn=[1,_sy,_tm],_to=[1,_st,_tn],_tp=[1,_so,_to],_tq=[1,_sj,_tp],_tr=[1,_se,_tq],_ts=[1,_s9,_tr],_tt=[1,_s4,_ts],_tu=[1,_rZ,_tt],_tv=[1,_rU,_tu],_tw=[1,_rP,_tv],_tx=[1,_rK,_tw],_ty=[1,_rF,_tx],_tz=[1,_rA,_ty],_tA=[1,_rv,_tz],_tB=[1,_rq,_tA],_tC=[1,_rl,_tB],_tD=[1,_rg,_tC],_tE=[1,_rb,_tD],_tF=[1,_r6,_tE],_tG=[1,_r1,_tF],_tH=[1,_qW,_tG],_tI=[1,_qR,_tH],_tJ=[1,_qM,_tI],_tK=[1,_qH,_tJ],_tL=new T(function(){return B(unCStr("SOH"));}),_tM=[0,1],_tN=function(_tO){return new F(function(){return _qu(_tL,function(_tP){return E(new T(function(){return B(A(_tO,[_tM]));}));});});},_tQ=new T(function(){return B(unCStr("SO"));}),_tR=[0,14],_tS=function(_tT){return new F(function(){return _qu(_tQ,function(_tU){return E(new T(function(){return B(A(_tT,[_tR]));}));});});},_tV=function(_tW){return new F(function(){return _nX(_tN,_tS,_tW);});},_tX=[1,_tV,_tK],_tY=new T(function(){return B(_qj(_tX));}),_tZ=[0,1114111],_u0=[0,34],_u1=[0,_u0,_hr],_u2=[0,39],_u3=[0,_u2,_hr],_u4=[0,92],_u5=[0,_u4,_hr],_u6=[0,_ra,_hr],_u7=[0,_rf,_hr],_u8=[0,_rz,_hr],_u9=[0,_rp,_hr],_ua=[0,_rE,_hr],_ub=[0,_rk,_hr],_uc=[0,_ru,_hr],_ud=[0,_qG,_hr],_ue=[0,_tM,_hr],_uf=[0,_qL,_hr],_ug=[0,_qQ,_hr],_uh=[0,_qV,_hr],_ui=[0,_r0,_hr],_uj=[0,_r5,_hr],_uk=[0,_ra,_hr],_ul=[0,_rf,_hr],_um=[0,_rk,_hr],_un=[0,_rp,_hr],_uo=[0,_ru,_hr],_up=[0,_rz,_hr],_uq=[0,_rE,_hr],_ur=[0,_tR,_hr],_us=[0,_rJ,_hr],_ut=[0,_rO,_hr],_uu=[0,_rT,_hr],_uv=[0,_rY,_hr],_uw=[0,_s3,_hr],_ux=[0,_s8,_hr],_uy=[0,_sd,_hr],_uz=[0,_si,_hr],_uA=[0,_sn,_hr],_uB=[0,_ss,_hr],_uC=[0,_sx,_hr],_uD=[0,_sC,_hr],_uE=[0,_sH,_hr],_uF=[0,_sM,_hr],_uG=[0,_sR,_hr],_uH=[0,_sW,_hr],_uI=[0,_t1,_hr],_uJ=function(_uK){return new F(function(){return _mO([0,function(_uL){switch(E(E(_uL)[1])){case 34:return E(new T(function(){return B(A(_uK,[_u1]));}));case 39:return E(new T(function(){return B(A(_uK,[_u3]));}));case 92:return E(new T(function(){return B(A(_uK,[_u5]));}));case 97:return E(new T(function(){return B(A(_uK,[_u6]));}));case 98:return E(new T(function(){return B(A(_uK,[_u7]));}));case 102:return E(new T(function(){return B(A(_uK,[_u8]));}));case 110:return E(new T(function(){return B(A(_uK,[_u9]));}));case 114:return E(new T(function(){return B(A(_uK,[_ua]));}));case 116:return E(new T(function(){return B(A(_uK,[_ub]));}));case 118:return E(new T(function(){return B(A(_uK,[_uc]));}));default:return [2];}}],new T(function(){return B(_mO(B(_nX(_pZ,_q2,function(_uM){return new F(function(){return _ok(_uM,function(_uN){var _uO=B(_pj(new T(function(){return B(_p9(E(_uM)[1]));}),_p8,_uN));return !B(_q9(_uO,_tZ))?[2]:B(A(_uK,[[0,new T(function(){var _uP=B(_q6(_uO));if(_uP>>>0>1114111){var _uQ=B(_q4(_uP));}else{var _uQ=[0,_uP];}var _uR=_uQ,_uS=_uR;return _uS;}),_hr]]));});});})),new T(function(){return B(_mO([0,function(_uT){return E(E(_uT)[1])==94?E([0,function(_uU){switch(E(E(_uU)[1])){case 64:return E(new T(function(){return B(A(_uK,[_ud]));}));case 65:return E(new T(function(){return B(A(_uK,[_ue]));}));case 66:return E(new T(function(){return B(A(_uK,[_uf]));}));case 67:return E(new T(function(){return B(A(_uK,[_ug]));}));case 68:return E(new T(function(){return B(A(_uK,[_uh]));}));case 69:return E(new T(function(){return B(A(_uK,[_ui]));}));case 70:return E(new T(function(){return B(A(_uK,[_uj]));}));case 71:return E(new T(function(){return B(A(_uK,[_uk]));}));case 72:return E(new T(function(){return B(A(_uK,[_ul]));}));case 73:return E(new T(function(){return B(A(_uK,[_um]));}));case 74:return E(new T(function(){return B(A(_uK,[_un]));}));case 75:return E(new T(function(){return B(A(_uK,[_uo]));}));case 76:return E(new T(function(){return B(A(_uK,[_up]));}));case 77:return E(new T(function(){return B(A(_uK,[_uq]));}));case 78:return E(new T(function(){return B(A(_uK,[_ur]));}));case 79:return E(new T(function(){return B(A(_uK,[_us]));}));case 80:return E(new T(function(){return B(A(_uK,[_ut]));}));case 81:return E(new T(function(){return B(A(_uK,[_uu]));}));case 82:return E(new T(function(){return B(A(_uK,[_uv]));}));case 83:return E(new T(function(){return B(A(_uK,[_uw]));}));case 84:return E(new T(function(){return B(A(_uK,[_ux]));}));case 85:return E(new T(function(){return B(A(_uK,[_uy]));}));case 86:return E(new T(function(){return B(A(_uK,[_uz]));}));case 87:return E(new T(function(){return B(A(_uK,[_uA]));}));case 88:return E(new T(function(){return B(A(_uK,[_uB]));}));case 89:return E(new T(function(){return B(A(_uK,[_uC]));}));case 90:return E(new T(function(){return B(A(_uK,[_uD]));}));case 91:return E(new T(function(){return B(A(_uK,[_uE]));}));case 92:return E(new T(function(){return B(A(_uK,[_uF]));}));case 93:return E(new T(function(){return B(A(_uK,[_uG]));}));case 94:return E(new T(function(){return B(A(_uK,[_uH]));}));case 95:return E(new T(function(){return B(A(_uK,[_uI]));}));default:return [2];}}]):[2];}],new T(function(){return B(A(_tY,[function(_uV){return new F(function(){return A(_uK,[[0,_uV,_hr]]);});}]));})));})));}));});},_uW=function(_uX){return new F(function(){return A(_uX,[_3u]);});},_uY=function(_uZ){var _v0=E(_uZ);if(!_v0[0]){return E(_uW);}else{var _v1=_v0[2],_v2=E(E(_v0[1])[1]);switch(_v2){case 9:return function(_v3){return [0,function(_v4){return E(new T(function(){return B(A(new T(function(){return B(_uY(_v1));}),[_v3]));}));}];};case 10:return function(_v5){return [0,function(_v6){return E(new T(function(){return B(A(new T(function(){return B(_uY(_v1));}),[_v5]));}));}];};case 11:return function(_v7){return [0,function(_v8){return E(new T(function(){return B(A(new T(function(){return B(_uY(_v1));}),[_v7]));}));}];};case 12:return function(_v9){return [0,function(_va){return E(new T(function(){return B(A(new T(function(){return B(_uY(_v1));}),[_v9]));}));}];};case 13:return function(_vb){return [0,function(_vc){return E(new T(function(){return B(A(new T(function(){return B(_uY(_v1));}),[_vb]));}));}];};case 32:return function(_vd){return [0,function(_ve){return E(new T(function(){return B(A(new T(function(){return B(_uY(_v1));}),[_vd]));}));}];};case 160:return function(_vf){return [0,function(_vg){return E(new T(function(){return B(A(new T(function(){return B(_uY(_v1));}),[_vf]));}));}];};default:var _vh=u_iswspace(_v2),_vi=_vh;return E(_vi)==0?E(_uW):function(_vj){return [0,function(_vk){return E(new T(function(){return B(A(new T(function(){return B(_uY(_v1));}),[_vj]));}));}];};}}},_vl=function(_vm){var _vn=new T(function(){return B(_vl(_vm));}),_vo=[1,function(_vp){return new F(function(){return A(_uY,[_vp,function(_vq){return E([0,function(_vr){return E(E(_vr)[1])==92?E(_vn):[2];}]);}]);});}];return new F(function(){return _mO([0,function(_vs){return E(E(_vs)[1])==92?E([0,function(_vt){var _vu=E(E(_vt)[1]);switch(_vu){case 9:return E(_vo);case 10:return E(_vo);case 11:return E(_vo);case 12:return E(_vo);case 13:return E(_vo);case 32:return E(_vo);case 38:return E(_vn);case 160:return E(_vo);default:var _vv=u_iswspace(_vu),_vw=_vv;return E(_vw)==0?[2]:E(_vo);}}]):[2];}],[0,function(_vx){var _vy=E(_vx);return E(_vy[1])==92?E(new T(function(){return B(_uJ(_vm));})):B(A(_vm,[[0,_vy,_hu]]));}]);});},_vz=function(_vA,_vB){return new F(function(){return _vl(function(_vC){var _vD=E(_vC),_vE=E(_vD[1]);if(E(_vE[1])==34){if(!E(_vD[2])){return E(new T(function(){return B(A(_vB,[[1,new T(function(){return B(A(_vA,[_E]));})]]));}));}else{return new F(function(){return _vz(function(_vF){return new F(function(){return A(_vA,[[1,_vE,_vF]]);});},_vB);});}}else{return new F(function(){return _vz(function(_vG){return new F(function(){return A(_vA,[[1,_vE,_vG]]);});},_vB);});}});});},_vH=new T(function(){return B(unCStr("_\'"));}),_vI=function(_vJ){var _vK=u_iswalnum(_vJ),_vL=_vK;return E(_vL)==0?B(_fJ(_fF,[0,_vJ],_vH)):true;},_vM=function(_vN){return new F(function(){return _vI(E(_vN)[1]);});},_vO=new T(function(){return B(unCStr(",;()[]{}`"));}),_vP=function(_vQ){return new F(function(){return A(_vQ,[_E]);});},_vR=function(_vS,_vT){var _vU=function(_vV){var _vW=E(_vV);if(!_vW[0]){return E(_vP);}else{var _vX=_vW[1];return !B(A(_vS,[_vX]))?E(_vP):function(_vY){return [0,function(_vZ){return E(new T(function(){return B(A(new T(function(){return B(_vU(_vW[2]));}),[function(_w0){return new F(function(){return A(_vY,[[1,_vX,_w0]]);});}]));}));}];};}};return [1,function(_w1){return new F(function(){return A(_vU,[_w1,_vT]);});}];},_w2=new T(function(){return B(unCStr(".."));}),_w3=new T(function(){return B(unCStr("::"));}),_w4=new T(function(){return B(unCStr("->"));}),_w5=[0,64],_w6=[1,_w5,_E],_w7=[0,126],_w8=[1,_w7,_E],_w9=new T(function(){return B(unCStr("=>"));}),_wa=[1,_w9,_E],_wb=[1,_w8,_wa],_wc=[1,_w6,_wb],_wd=[1,_w4,_wc],_we=new T(function(){return B(unCStr("<-"));}),_wf=[1,_we,_wd],_wg=[0,124],_wh=[1,_wg,_E],_wi=[1,_wh,_wf],_wj=[1,_u4,_E],_wk=[1,_wj,_wi],_wl=[0,61],_wm=[1,_wl,_E],_wn=[1,_wm,_wk],_wo=[1,_w3,_wn],_wp=[1,_w2,_wo],_wq=function(_wr){return new F(function(){return _mO([1,function(_ws){return E(_ws)[0]==0?E(new T(function(){return B(A(_wr,[_oh]));})):[2];}],new T(function(){return B(_mO([0,function(_wt){return E(E(_wt)[1])==39?E([0,function(_wu){var _wv=E(_wu);switch(E(_wv[1])){case 39:return [2];case 92:return E(new T(function(){return B(_uJ(function(_ww){var _wx=E(_ww);return new F(function(){return (function(_wy,_wz){var _wA=new T(function(){return B(A(_wr,[[0,_wy]]));});return !E(_wz)?E(E(_wy)[1])==39?[2]:[0,function(_wB){return E(E(_wB)[1])==39?E(_wA):[2];}]:[0,function(_wC){return E(E(_wC)[1])==39?E(_wA):[2];}];})(_wx[1],_wx[2]);});}));}));default:return [0,function(_wD){return E(E(_wD)[1])==39?E(new T(function(){return B(A(_wr,[[0,_wv]]));})):[2];}];}}]):[2];}],new T(function(){return B(_mO([0,function(_wE){return E(E(_wE)[1])==34?E(new T(function(){return B(_vz(_a2,_wr));})):[2];}],new T(function(){return B(_mO([0,function(_wF){return !B(_fJ(_fF,_wF,_vO))?[2]:B(A(_wr,[[2,[1,_wF,_E]]]));}],new T(function(){return B(_mO([0,function(_wG){if(!B(_fJ(_fF,_wG,_pM))){return [2];}else{return new F(function(){return _vR(_pN,function(_wH){var _wI=[1,_wG,_wH];return !B(_fJ(_nD,_wI,_wp))?B(A(_wr,[[4,_wI]])):B(A(_wr,[[2,_wI]]));});});}}],new T(function(){return B(_mO([0,function(_wJ){var _wK=E(_wJ),_wL=_wK[1],_wM=u_iswalpha(_wL),_wN=_wM;if(!E(_wN)){if(E(_wL)==95){return new F(function(){return _vR(_vM,function(_wO){return new F(function(){return A(_wr,[[3,[1,_wK,_wO]]]);});});});}else{return [2];}}else{return new F(function(){return _vR(_vM,function(_wP){return new F(function(){return A(_wr,[[3,[1,_wK,_wP]]]);});});});}}],new T(function(){return B(_nX(_pR,_pH,_wr));})));})));})));})));})));}));});},_wQ=function(_wR){return [1,function(_wS){return new F(function(){return A(_uY,[_wS,function(_wT){return E(new T(function(){return B(_wq(_wR));}));}]);});}];},_wU=[0,0],_wV=function(_wW,_wX){return new F(function(){return _wQ(function(_wY){var _wZ=E(_wY);if(_wZ[0]==2){var _x0=E(_wZ[1]);return _x0[0]==0?[2]:E(E(_x0[1])[1])==40?E(_x0[2])[0]==0?E(new T(function(){return B(A(_wW,[_wU,function(_x1){return new F(function(){return _wQ(function(_x2){var _x3=E(_x2);if(_x3[0]==2){var _x4=E(_x3[1]);return _x4[0]==0?[2]:E(E(_x4[1])[1])==41?E(_x4[2])[0]==0?E(new T(function(){return B(A(_wX,[_x1]));})):[2]:[2];}else{return [2];}});});}]));})):[2]:[2];}else{return [2];}});});},_x5=function(_x6,_x7,_x8){var _x9=function(_xa,_xb){return new F(function(){return _mO(B(_wQ(function(_xc){var _xd=E(_xc);if(_xd[0]==4){var _xe=E(_xd[1]);if(!_xe[0]){return new F(function(){return A(_x6,[_xd,_xa,_xb]);});}else{return E(E(_xe[1])[1])==45?E(_xe[2])[0]==0?E([1,function(_xf){return new F(function(){return A(_uY,[_xf,function(_xg){return E(new T(function(){return B(_wq(function(_xh){return new F(function(){return A(_x6,[_xh,_xa,function(_xi){return new F(function(){return A(_xb,[new T(function(){return B(_p3(_xi));})]);});}]);});}));}));}]);});}]):B(A(_x6,[_xd,_xa,_xb])):B(A(_x6,[_xd,_xa,_xb]));}}else{return new F(function(){return A(_x6,[_xd,_xa,_xb]);});}})),new T(function(){return B(_wV(_x9,_xb));}));});};return new F(function(){return _x9(_x7,_x8);});},_xj=function(_xk,_xl){return [2];},_xm=function(_xn,_xo){return new F(function(){return _xj(_xn,_xo);});},_xp=function(_xq){var _xr=E(_xq);return _xr[0]==0?[1,new T(function(){return B(_pj(new T(function(){return B(_p9(E(_xr[1])[1]));}),_p8,_xr[2]));})]:E(_xr[2])[0]==0?E(_xr[3])[0]==0?[1,new T(function(){return B(_pj(_p7,_p8,_xr[1]));})]:[0]:[0];},_xs=function(_xt){var _xu=E(_xt);if(_xu[0]==5){var _xv=B(_xp(_xu[1]));return _xv[0]==0?E(_xj):function(_xw,_xx){return new F(function(){return A(_xx,[_xv[1]]);});};}else{return E(_xm);}},_xy=function(_xz){return [1,function(_xA){return new F(function(){return A(_uY,[_xA,function(_xB){return E([3,_xz,_nP]);}]);});}];},_xC=new T(function(){return B(_x5(_xs,_wU,_xy));}),_xD=function(_xE){while(1){var _xF=(function(_xG){var _xH=E(_xG);if(!_xH[0]){return [0];}else{var _xI=_xH[2],_xJ=E(_xH[1]);if(!E(_xJ[2])[0]){return [1,_xJ[1],new T(function(){return B(_xD(_xI));})];}else{_xE=_xI;return null;}}})(_xE);if(_xF!=null){return _xF;}}},_xK=function(_xL){return function(_xM,_xN,_xO,_xP,_xQ){return new F(function(){return A(_xP,[new T(function(){return !B(_88(_E,_xL))?!B(_kT(_li,_xL))?[2,[1,_f1,_xL]]:[3,new T(function(){var _xR=B(_xD(B(_mE(_xC,[1,_f1,_xL]))));return _xR[0]==0?E(_ln):E(_xR[2])[0]==0?E(_xR[1]):E(_lp);})]:E(_ll);}),_xM,new T(function(){return [0,E(E(_xM)[2]),_E];})]);});};},_xS=function(_xT,_xU,_xV,_xW,_xX){var _xY=E(_xT),_xZ=E(_xY[2]);return new F(function(){return _gL(_d,_e8,_kr,_xY[1],_xZ[1],_xZ[2],_xZ[3],_xY[3],_xU,_xX);});},_y0=function(_y1,_y2,_y3,_y4,_y5){return new F(function(){return _bn(_xS,_y1,function(_y6,_y7,_y8){return new F(function(){return A(_xK,[_y6,_y7,_y2,_y3,function(_y9,_ya,_yb){return new F(function(){return A(_y2,[_y9,_ya,new T(function(){return B(_bh(_y8,_yb));})]);});},function(_yc){return new F(function(){return A(_y3,[new T(function(){return B(_bh(_y8,_yc));})]);});}]);});},_y3,function(_yd,_ye,_yf){return new F(function(){return A(_xK,[_yd,_ye,_y2,_y3,function(_yg,_yh,_yi){return new F(function(){return A(_y4,[_yg,_yh,new T(function(){return B(_bh(_yf,_yi));})]);});},function(_yj){return new F(function(){return A(_y5,[new T(function(){return B(_bh(_yf,_yj));})]);});}]);});},_y5);});},_yk=function(_yl){return function(_ym,_yn,_yo,_yp,_yq){return new F(function(){return A(_yp,[[3,new T(function(){var _yr=B(_xD(B(_mE(_xC,_yl))));return _yr[0]==0?E(_ln):E(_yr[2])[0]==0?E(_yr[1]):E(_lp);})],_ym,new T(function(){return [0,E(E(_ym)[2]),_E];})]);});};},_ys=function(_yt,_yu,_yv,_yw,_yx){var _yy=function(_yz,_yA,_yB){return new F(function(){return _eo(_yz,_yA,_yu,_yv,function(_yC,_yD,_yE){return new F(function(){return A(_yw,[_yC,_yD,new T(function(){return B(_bh(_yB,_yE));})]);});});});},_yF=function(_yG,_yH,_yI){return new F(function(){return _eo(_yG,_yH,_yu,_yv,function(_yJ,_yK,_yL){return new F(function(){return A(_yu,[_yJ,_yK,new T(function(){return B(_bh(_yI,_yL));})]);});});});},_yM=function(_yN){var _yO=function(_yP){var _yQ=function(_yR){var _yS=function(_yT,_yU,_yV){return new F(function(){return _yy(_yT,_yU,new T(function(){var _yW=E(_yN),_yX=E(_yP),_yY=E(_yR),_yZ=E(_yV),_z0=B(_am(_yY[1],_yY[2],_yZ[1],_yZ[2])),_z1=B(_am(_yX[1],_yX[2],_z0[1],_z0[2])),_z2=B(_am(_yW[1],_yW[2],_z1[1],_z1[2]));return [0,E(_z2[1]),_z2[2]];}));});},_z3=function(_z4){var _z5=function(_z6){var _z7=function(_z8){return new F(function(){return A(_yx,[new T(function(){var _z9=E(_yN),_za=E(_yP),_zb=E(_yR),_zc=E(_z4),_zd=E(_z6),_ze=E(_z8),_zf=B(_am(_zd[1],_zd[2],_ze[1],_ze[2])),_zg=B(_am(_zc[1],_zc[2],_zf[1],_zf[2])),_zh=B(_am(_zb[1],_zb[2],_zg[1],_zg[2])),_zi=B(_am(_za[1],_za[2],_zh[1],_zh[2])),_zj=B(_am(_z9[1],_z9[2],_zi[1],_zi[2]));return [0,E(_zj[1]),_zj[2]];})]);});};return new F(function(){return _bQ(_ks,_yt,function(_zk,_zl,_zm){return new F(function(){return A(_hx,[_zk,_zl,_yF,_yv,function(_zn,_zo,_zp){return new F(function(){return _yF(_zn,_zo,new T(function(){return B(_bh(_zm,_zp));}));});},function(_zq){return new F(function(){return A(_yv,[new T(function(){return B(_bh(_zm,_zq));})]);});}]);});},_yv,function(_zr,_zs,_zt){return new F(function(){return A(_hx,[_zr,_zs,_yF,_yv,function(_zu,_zv,_zw){return new F(function(){return _yS(_zu,_zv,new T(function(){var _zx=E(_z4),_zy=E(_z6),_zz=E(_zt),_zA=E(_zw),_zB=B(_am(_zz[1],_zz[2],_zA[1],_zA[2])),_zC=B(_am(_zy[1],_zy[2],_zB[1],_zB[2])),_zD=B(_am(_zx[1],_zx[2],_zC[1],_zC[2]));return [0,E(_zD[1]),_zD[2]];}));});},function(_zE){return new F(function(){return _z7(new T(function(){var _zF=E(_zt),_zG=E(_zE),_zH=B(_am(_zF[1],_zF[2],_zG[1],_zG[2]));return [0,E(_zH[1]),_zH[2]];}));});}]);});},_z7);});};return new F(function(){return _bQ(_kN,_yt,function(_zI,_zJ,_zK){return new F(function(){return A(_yk,[_zI,_zJ,_yF,_yv,function(_zL,_zM,_zN){return new F(function(){return _yF(_zL,_zM,new T(function(){return B(_bh(_zK,_zN));}));});},function(_zO){return new F(function(){return A(_yv,[new T(function(){return B(_bh(_zK,_zO));})]);});}]);});},_yv,function(_zP,_zQ,_zR){return new F(function(){return A(_yk,[_zP,_zQ,_yF,_yv,function(_zS,_zT,_zU){return new F(function(){return _yS(_zS,_zT,new T(function(){var _zV=E(_z4),_zW=E(_zR),_zX=E(_zU),_zY=B(_am(_zW[1],_zW[2],_zX[1],_zX[2])),_zZ=B(_am(_zV[1],_zV[2],_zY[1],_zY[2]));return [0,E(_zZ[1]),_zZ[2]];}));});},function(_A0){return new F(function(){return _z5(new T(function(){return B(_bh(_zR,_A0));}));});}]);});},_z5);});};return new F(function(){return A(_f2,[_yt,function(_A1,_A2,_A3){return new F(function(){return _y0(_A2,_yF,_yv,function(_A4,_A5,_A6){return new F(function(){return _yF(_A4,_A5,new T(function(){return B(_bh(_A3,_A6));}));});},function(_A7){return new F(function(){return A(_yv,[new T(function(){return B(_bh(_A3,_A7));})]);});});});},_yv,function(_A8,_A9,_Aa){return new F(function(){return _y0(_A9,_yF,_yv,function(_Ab,_Ac,_Ad){return new F(function(){return _yS(_Ab,_Ac,new T(function(){var _Ae=E(_Aa),_Af=E(_Ad),_Ag=B(_am(_Ae[1],_Ae[2],_Af[1],_Af[2]));return [0,E(_Ag[1]),_Ag[2]];}));});},function(_Ah){return new F(function(){return _z3(new T(function(){return B(_bh(_Aa,_Ah));}));});});});},_z3]);});};return new F(function(){return A(_f4,[_yt,function(_Ai,_Aj,_Ak){return new F(function(){return _hJ(_Aj,_yF,_yv,function(_Al,_Am,_An){return new F(function(){return _yF(_Al,_Am,new T(function(){return B(_bh(_Ak,_An));}));});},function(_Ao){return new F(function(){return A(_yv,[new T(function(){return B(_bh(_Ak,_Ao));})]);});});});},_yv,function(_Ap,_Aq,_Ar){return new F(function(){return _hJ(_Aq,_yF,_yv,function(_As,_At,_Au){return new F(function(){return _yy(_As,_At,new T(function(){var _Av=E(_yN),_Aw=E(_yP),_Ax=E(_Ar),_Ay=E(_Au),_Az=B(_am(_Ax[1],_Ax[2],_Ay[1],_Ay[2])),_AA=B(_am(_Aw[1],_Aw[2],_Az[1],_Az[2])),_AB=B(_am(_Av[1],_Av[2],_AA[1],_AA[2]));return [0,E(_AB[1]),_AB[2]];}));});},function(_AC){return new F(function(){return _yQ(new T(function(){return B(_bh(_Ar,_AC));}));});});});},_yQ]);});};return new F(function(){return A(_hE,[_yt,function(_AD,_AE,_AF){return new F(function(){return _h7(_AE,_yF,_yv,function(_AG,_AH,_AI){return new F(function(){return _yF(_AG,_AH,new T(function(){return B(_bh(_AF,_AI));}));});},function(_AJ){return new F(function(){return A(_yv,[new T(function(){return B(_bh(_AF,_AJ));})]);});});});},_yv,function(_AK,_AL,_AM){return new F(function(){return _h7(_AL,_yF,_yv,function(_AN,_AO,_AP){return new F(function(){return _yy(_AN,_AO,new T(function(){var _AQ=E(_yN),_AR=E(_AM),_AS=E(_AP),_AT=B(_am(_AR[1],_AR[2],_AS[1],_AS[2])),_AU=B(_am(_AQ[1],_AQ[2],_AT[1],_AT[2]));return [0,E(_AU[1]),_AU[2]];}));});},function(_AV){return new F(function(){return _yO(new T(function(){return B(_bh(_AM,_AV));}));});});});},_yO]);});};return new F(function(){return A(_hG,[_yt,function(_AW,_AX,_AY){return new F(function(){return _k7(_AX,_yF,_yv,function(_AZ,_B0,_B1){return new F(function(){return _yF(_AZ,_B0,new T(function(){return B(_bh(_AY,_B1));}));});},function(_B2){return new F(function(){return A(_yv,[new T(function(){return B(_bh(_AY,_B2));})]);});});});},_yv,function(_B3,_B4,_B5){return new F(function(){return _k7(_B4,_yF,_yv,function(_B6,_B7,_B8){return new F(function(){return _yy(_B6,_B7,new T(function(){return B(_bh(_B5,_B8));}));});},function(_B9){return new F(function(){return _yM(new T(function(){return B(_bh(_B5,_B9));}));});});});},_yM]);});},_hP=function(_Ba,_Bb,_Bc,_Bd,_Be){return new F(function(){return _aJ(_ei,_Ba,function(_Bf,_Bg,_Bh){return new F(function(){return _ys(_Bg,_Bb,_Bc,function(_Bi,_Bj,_Bk){return new F(function(){return A(_Bb,[_Bi,_Bj,new T(function(){return B(_bh(_Bh,_Bk));})]);});},function(_Bl){return new F(function(){return A(_Bc,[new T(function(){return B(_bh(_Bh,_Bl));})]);});});});},_Bc,function(_Bm,_Bn,_Bo){return new F(function(){return _ys(_Bn,_Bb,_Bc,function(_Bp,_Bq,_Br){return new F(function(){return A(_Bd,[_Bp,_Bq,new T(function(){return B(_bh(_Bo,_Br));})]);});},function(_Bs){return new F(function(){return A(_Be,[new T(function(){return B(_bh(_Bo,_Bs));})]);});});});});});},_Bt=function(_Bu,_Bv,_){var _Bw=jsGet(_Bu,E(_aj)[1]),_Bx=_Bw,_By=new T(function(){return fromJSStr(_Bx);}),_Bz=B(_3j(_d,_hP,_3u,_ak,_By));if(!_Bz[0]){var _BA=B(_a7(_9O,_9N,_)),_BB=_BA;return _Bv;}else{var _BC=new T(function(){return B(_7v(_Bz[1],_Bv));}),_BD=B(_a7(_9O,new T(function(){return B(_3v(B(_f(_By,new T(function(){return B(_f(_9M,new T(function(){var _BE=E(_BC);return _BE[0]==0?B(_T(_BE[1])):B(_T(_BE[1]));})));})))));}),_)),_BF=_BD;return new T(function(){var _BG=E(_BC);return _BG[0]==0?E(_Bv):E(_BG[2]);});}},_BH=new T(function(){return [0,"keypress"];}),_BI=function(_BJ,_BK){while(1){var _BL=E(_BK);if(!_BL[0]){return E(_BJ);}else{var _BM=E(_BL[1]),_BN=B(_54(_BM[1],_BM[2],_BJ));_BK=_BL[2];_BJ=_BN;continue;}}},_BO=function(_BP,_BQ){return [0,1,E(E(_BP)),_BQ,E(_3H),E(_3H)];},_BR=function(_BS,_BT,_BU){var _BV=E(_BU);if(!_BV[0]){return new F(function(){return _4p(_BV[2],_BV[3],_BV[4],B(_BR(_BS,_BT,_BV[5])));});}else{return new F(function(){return _BO(_BS,_BT);});}},_BW=function(_BX,_BY,_BZ){var _C0=E(_BZ);if(!_C0[0]){return new F(function(){return _3K(_C0[2],_C0[3],B(_BW(_BX,_BY,_C0[4])),_C0[5]);});}else{return new F(function(){return _BO(_BX,_BY);});}},_C1=function(_C2,_C3,_C4,_C5,_C6,_C7,_C8,_C9){var _Ca=E(_C4);if(!_Ca[0]){var _Cb=_Ca[1],_Cc=_Ca[2],_Cd=_Ca[3],_Ce=_Ca[4],_Cf=_Ca[5];if((imul(3,_Cb)|0)>=_C5){if((imul(3,_C5)|0)>=_Cb){return [0,(_Cb+_C5|0)+1|0,E(E(_C2)),_C3,E(_Ca),E([0,_C5,E(_C6),_C7,E(_C8),E(_C9)])];}else{return new F(function(){return _4p(_Cc,_Cd,_Ce,B(_C1(_C2,_C3,_Cf,_C5,_C6,_C7,_C8,_C9)));});}}else{return new F(function(){return _3K(_C6,_C7,B(_Cg(_C2,_C3,_Cb,_Cc,_Cd,_Ce,_Cf,_C8)),_C9);});}}else{return new F(function(){return _BW(_C2,_C3,[0,_C5,E(_C6),_C7,E(_C8),E(_C9)]);});}},_Cg=function(_Ch,_Ci,_Cj,_Ck,_Cl,_Cm,_Cn,_Co){var _Cp=E(_Co);if(!_Cp[0]){var _Cq=_Cp[1],_Cr=_Cp[2],_Cs=_Cp[3],_Ct=_Cp[4],_Cu=_Cp[5];if((imul(3,_Cj)|0)>=_Cq){if((imul(3,_Cq)|0)>=_Cj){return [0,(_Cj+_Cq|0)+1|0,E(E(_Ch)),_Ci,E([0,_Cj,E(_Ck),_Cl,E(_Cm),E(_Cn)]),E(_Cp)];}else{return new F(function(){return _4p(_Ck,_Cl,_Cm,B(_C1(_Ch,_Ci,_Cn,_Cq,_Cr,_Cs,_Ct,_Cu)));});}}else{return new F(function(){return _3K(_Cr,_Cs,B(_Cg(_Ch,_Ci,_Cj,_Ck,_Cl,_Cm,_Cn,_Ct)),_Cu);});}}else{return new F(function(){return _BR(_Ch,_Ci,[0,_Cj,E(_Ck),_Cl,E(_Cm),E(_Cn)]);});}},_Cv=function(_Cw,_Cx,_Cy,_Cz){var _CA=E(_Cy);if(!_CA[0]){var _CB=_CA[1],_CC=_CA[2],_CD=_CA[3],_CE=_CA[4],_CF=_CA[5],_CG=E(_Cz);if(!_CG[0]){var _CH=_CG[1],_CI=_CG[2],_CJ=_CG[3],_CK=_CG[4],_CL=_CG[5];if((imul(3,_CB)|0)>=_CH){if((imul(3,_CH)|0)>=_CB){return [0,(_CB+_CH|0)+1|0,E(E(_Cw)),_Cx,E(_CA),E(_CG)];}else{return new F(function(){return _4p(_CC,_CD,_CE,B(_C1(_Cw,_Cx,_CF,_CH,_CI,_CJ,_CK,_CL)));});}}else{return new F(function(){return _3K(_CI,_CJ,B(_Cg(_Cw,_Cx,_CB,_CC,_CD,_CE,_CF,_CK)),_CL);});}}else{return new F(function(){return _BR(_Cw,_Cx,_CA);});}}else{return new F(function(){return _BW(_Cw,_Cx,_Cz);});}},_CM=function(_CN,_CO,_CP,_CQ){var _CR=E(_CN);if(_CR==1){var _CS=E(_CQ);return _CS[0]==0?[0,new T(function(){return [0,1,E(E(_CO)),_CP,E(_3H),E(_3H)];}),_E,_E]:B(_3A(_CO,E(_CS[1])[1]))==0?[0,new T(function(){return [0,1,E(E(_CO)),_CP,E(_3H),E(_3H)];}),_CS,_E]:[0,new T(function(){return [0,1,E(E(_CO)),_CP,E(_3H),E(_3H)];}),_E,_CS];}else{var _CT=B(_CM(_CR>>1,_CO,_CP,_CQ)),_CU=_CT[1],_CV=_CT[3],_CW=E(_CT[2]);if(!_CW[0]){return [0,_CU,_E,_CV];}else{var _CX=E(_CW[1]),_CY=_CX[1],_CZ=_CX[2],_D0=E(_CW[2]);if(!_D0[0]){return [0,new T(function(){return B(_BR(_CY,_CZ,_CU));}),_E,_CV];}else{var _D1=E(_D0[1]),_D2=_D1[1];if(!B(_3A(_CY,_D2))){var _D3=B(_CM(_CR>>1,_D2,_D1[2],_D0[2]));return [0,new T(function(){return B(_Cv(_CY,_CZ,_CU,_D3[1]));}),_D3[2],_D3[3]];}else{return [0,_CU,_E,_CW];}}}}},_D4=function(_D5,_D6,_D7){while(1){var _D8=E(_D7);if(!_D8[0]){return E(_D6);}else{var _D9=E(_D8[1]),_Da=_D9[1],_Db=_D9[2],_Dc=E(_D8[2]);if(!_Dc[0]){return new F(function(){return _BR(_Da,_Db,_D6);});}else{var _Dd=E(_Dc[1]),_De=_Dd[1];if(!B(_3A(_Da,_De))){var _Df=B(_CM(_D5,_De,_Dd[2],_Dc[2])),_Dg=_Df[1],_Dh=E(_Df[3]);if(!_Dh[0]){var _Di=_D5<<1,_Dj=B(_Cv(_Da,_Db,_D6,_Dg));_D7=_Df[2];_D5=_Di;_D6=_Dj;continue;}else{return new F(function(){return _BI(B(_Cv(_Da,_Db,_D6,_Dg)),_Dh);});}}else{return new F(function(){return _BI(_D6,_D8);});}}}}},_Dk=function(_Dl){var _Dm=E(_Dl);if(!_Dm[0]){return [1];}else{var _Dn=E(_Dm[1]),_Do=_Dn[1],_Dp=_Dn[2],_Dq=E(_Dm[2]);if(!_Dq[0]){return [0,1,E(E(_Do)),_Dp,E(_3H),E(_3H)];}else{if(!B(_3A(_Do,E(_Dq[1])[1]))){return new F(function(){return _D4(1,[0,1,E(E(_Do)),_Dp,E(_3H),E(_3H)],_Dq);});}else{return new F(function(){return _BI([0,1,E(E(_Do)),_Dp,E(_3H),E(_3H)],_Dq);});}}}},_Dr=function(_Ds){var _Dt=E(_Ds);return _Dt[0]==0?[6]:[0,[2,_Dt[1]],new T(function(){return B(_Dr(_Dt[2]));})];},_Du=[0,98],_Dv=[1,_Du,_E],_Dw=[1,_Dv,_E],_Dx=[0,97],_Dy=[1,_Dx,_E],_Dz=[1,_Dy,_Dw],_DA=new T(function(){return B(_Dr(_Dz));}),_DB=new T(function(){return B(_mB("Model.hs:99:45-76|lambda"));}),_DC=function(_DD,_DE){var _DF=E(_DE);return _DF[0]==0?[0]:[1,new T(function(){return B(A(_DD,[_DF[1]]));}),new T(function(){return B(_DC(_DD,_DF[2]));})];},_DG=function(_DH){var _DI=B(_DC(function(_DJ){return new F(function(){return _8i(_DH,_DJ);});},_Dz));if(!_DI[0]){return E(_DB);}else{var _DK=E(_DI[1]);if(_DK[0]==3){var _DL=E(_DI[2]);if(!_DL[0]){return E(_DB);}else{var _DM=E(_DL[1]);return _DM[0]==3?E(_DL[2])[0]==0?[3,new T(function(){return B(_pb(_DK[1],_DM[1]));})]:E(_DB):E(_DB);}}else{return E(_DB);}}},_DN=[7,_DG,_DA],_DO=[0,42],_DP=[1,_DO,_E],_DQ=[0,_DP,_DN],_DR=[1,_Du,_E],_DS=[1,_DR,_E],_DT=[1,_Dx,_E],_DU=[1,_DT,_DS],_DV=new T(function(){return B(_Dr(_DU));}),_DW=function(_DX,_DY){return !E(_DX)?!E(_DY)?true:false:E(_DY);},_DZ=function(_E0,_E1){var _E2=E(_E0);if(!_E2[0]){var _E3=_E2[1],_E4=E(_E1);return _E4[0]==0?_E3==_E4[1]:I_compareInt(_E4[1],_E3)==0?true:false;}else{var _E5=_E2[1],_E6=E(_E1);return _E6[0]==0?I_compareInt(_E5,_E6[1])==0?true:false:I_compare(_E5,_E6[1])==0?true:false;}},_E7=function(_E8,_E9){while(1){var _Ea=E(_E8);switch(_Ea[0]){case 0:var _Eb=E(_E9);if(!_Eb[0]){if(!B(_E7(_Ea[1],_Eb[1]))){return false;}else{_E8=_Ea[2];_E9=_Eb[2];continue;}}else{return false;}break;case 1:var _Ec=E(_E9);return _Ec[0]==1?B(_88(_Ea[1],_Ec[1])):false;case 2:var _Ed=E(_E9);return _Ed[0]==2?B(_88(_Ea[1],_Ed[1])):false;case 3:var _Ee=E(_E9);return _Ee[0]==3?B(_DZ(_Ea[1],_Ee[1])):false;case 4:var _Ef=E(_E9);return _Ef[0]==4?B(_DW(_Ea[1],_Ef[1])):false;case 5:var _Eg=E(_E9);return _Eg[0]==5?B(_eH(_Ea[1],_Eg[1])):false;case 6:return E(_E9)[0]==6?true:false;default:return false;}}},_Eh=new T(function(){return B(_mB("Model.hs:100:45-70|lambda"));}),_Ei=function(_Ej){var _Ek=B(_DC(function(_DJ){return new F(function(){return _8i(_Ej,_DJ);});},_DU));if(!_Ek[0]){return E(_Eh);}else{var _El=E(_Ek[2]);return _El[0]==0?E(_Eh):E(_El[2])[0]==0?[4,new T(function(){return B(_E7(_Ek[1],_El[1]));})]:E(_Eh);}},_Em=[7,_Ei,_DV],_En=[0,61],_Eo=[1,_En,_E],_Ep=[0,_Eo,_Em],_Eq=[1,_Dx,_E],_Er=[1,_Eq,_E],_Es=new T(function(){return B(_Dr(_Er));}),_Et=new T(function(){return B(_mB("Model.hs:101:42-63|lambda"));}),_Eu=function(_Ev){var _Ew=B(_8i(_Ev,_Eq));return _Ew[0]==0?E(_Ew[1]):E(_Et);},_Ex=[7,_Eu,_Es],_Ey=new T(function(){return B(unCStr("car"));}),_Ez=[0,_Ey,_Ex],_EA=[1,_Dx,_E],_EB=[1,_EA,_E],_EC=new T(function(){return B(_Dr(_EB));}),_ED=new T(function(){return B(_mB("Model.hs:102:42-63|lambda"));}),_EE=function(_EF){var _EG=B(_8i(_EF,_EA));return _EG[0]==0?E(_EG[2]):E(_ED);},_EH=[7,_EE,_EC],_EI=new T(function(){return B(unCStr("cdr"));}),_EJ=[0,_EI,_EH],_EK=new T(function(){return B(_Dr(_E));}),_EL=function(_EM){var _EN=E(_EM);if(!_EN[0]){return [6];}else{var _EO=E(_EN[1]);return [0,[0,[2,_EO[1]],_EO[2]],new T(function(){return B(_EL(_EN[2]));})];}},_EP=function(_EQ){var _ER=E(_EQ);return _ER[0]==0?[6]:[0,new T(function(){return B(_EL(B(_6e(_E,_ER[1]))));}),new T(function(){return B(_EP(_ER[2]));})];},_ES=function(_ET){return new F(function(){return _EP(_ET);});},_EU=[7,_ES,_EK],_EV=new T(function(){return B(unCStr("the-env"));}),_EW=[0,_EV,_EU],_EX=[1,_EW,_E],_EY=[1,_Du,_E],_EZ=[1,_EY,_E],_F0=[1,_Dx,_E],_F1=[1,_F0,_EZ],_F2=new T(function(){return B(_Dr(_F1));}),_F3=new T(function(){return B(_mB("Model.hs:103:48-68|lambda"));}),_F4=function(_F5){var _F6=B(_DC(function(_DJ){return new F(function(){return _8i(_F5,_DJ);});},_F1));if(!_F6[0]){return E(_F3);}else{var _F7=E(_F6[2]);return _F7[0]==0?E(_F3):E(_F7[2])[0]==0?[0,_F6[1],_F7[1]]:E(_F3);}},_F8=[7,_F4,_F2],_F9=new T(function(){return B(unCStr("cons"));}),_Fa=[0,_F9,_F8],_Fb=[1,_Fa,_EX],_Fc=[1,_EJ,_Fb],_Fd=[1,_Ez,_Fc],_Fe=[1,_Ep,_Fd],_Ff=[1,_DQ,_Fe],_Fg=[1,_Du,_E],_Fh=[1,_Fg,_E],_Fi=[1,_Dx,_E],_Fj=[1,_Fi,_Fh],_Fk=new T(function(){return B(_Dr(_Fj));}),_Fl=function(_Fm,_Fn){if(_Fm<=0){if(_Fm>=0){return new F(function(){return quot(_Fm,_Fn);});}else{if(_Fn<=0){return new F(function(){return quot(_Fm,_Fn);});}else{return quot(_Fm+1|0,_Fn)-1|0;}}}else{if(_Fn>=0){if(_Fm>=0){return new F(function(){return quot(_Fm,_Fn);});}else{if(_Fn<=0){return new F(function(){return quot(_Fm,_Fn);});}else{return quot(_Fm+1|0,_Fn)-1|0;}}}else{return quot(_Fm-1|0,_Fn)-1|0;}}},_Fo=function(_Fp,_Fq){while(1){var _Fr=E(_Fp);if(!_Fr[0]){var _Fs=E(_Fr[1]);if(_Fs==(-2147483648)){_Fp=[1,I_fromInt(-2147483648)];continue;}else{var _Ft=E(_Fq);if(!_Ft[0]){return [0,B(_Fl(_Fs,_Ft[1]))];}else{_Fp=[1,I_fromInt(_Fs)];_Fq=_Ft;continue;}}}else{var _Fu=_Fr[1],_Fv=E(_Fq);return _Fv[0]==0?[0,I_toInt(I_div(_Fu,I_fromInt(_Fv[1])))]:[1,I_div(_Fu,_Fv[1])];}}},_Fw=new T(function(){return B(unCStr("ArithException"));}),_Fx=new T(function(){return B(unCStr("GHC.Exception"));}),_Fy=new T(function(){return B(unCStr("base"));}),_Fz=new T(function(){var _FA=hs_wordToWord64(4194982440),_FB=_FA,_FC=hs_wordToWord64(3110813675),_FD=_FC;return [0,_FB,_FD,[0,_FB,_FD,_Fy,_Fx,_Fw],_E];}),_FE=function(_FF){return E(_Fz);},_FG=function(_FH){var _FI=E(_FH);return new F(function(){return _lE(B(_lA(_FI[1])),_FE,_FI[2]);});},_FJ=new T(function(){return B(unCStr("arithmetic underflow"));}),_FK=new T(function(){return B(unCStr("arithmetic overflow"));}),_FL=new T(function(){return B(unCStr("Ratio has zero denominator"));}),_FM=new T(function(){return B(unCStr("denormal"));}),_FN=new T(function(){return B(unCStr("divide by zero"));}),_FO=new T(function(){return B(unCStr("loss of precision"));}),_FP=function(_FQ){switch(E(_FQ)){case 0:return E(_FK);case 1:return E(_FJ);case 2:return E(_FO);case 3:return E(_FN);case 4:return E(_FM);default:return E(_FL);}},_FR=function(_FS){return new F(function(){return _f(_FJ,_FS);});},_FT=function(_FS){return new F(function(){return _f(_FK,_FS);});},_FU=function(_FS){return new F(function(){return _f(_FL,_FS);});},_FV=function(_FS){return new F(function(){return _f(_FM,_FS);});},_FW=function(_FS){return new F(function(){return _f(_FN,_FS);});},_FX=function(_FS){return new F(function(){return _f(_FO,_FS);});},_FY=function(_FZ){switch(E(_FZ)){case 0:return E(_FT);case 1:return E(_FR);case 2:return E(_FX);case 3:return E(_FW);case 4:return E(_FV);default:return E(_FU);}},_G0=function(_G1,_G2){return new F(function(){return _5M(_FY,_G1,_G2);});},_G3=function(_G4,_G5){switch(E(_G5)){case 0:return E(_FT);case 1:return E(_FR);case 2:return E(_FX);case 3:return E(_FW);case 4:return E(_FV);default:return E(_FU);}},_G6=[0,_G3,_FP,_G0],_G7=new T(function(){return [0,_FE,_G6,_G8,_FG];}),_G8=function(_FS){return [0,_G7,_FS];},_G9=3,_Ga=new T(function(){return B(_md(_G9,_G8));}),_Gb=[0,0],_Gc=function(_Gd,_Ge){return !B(_DZ(_Ge,_Gb))?B(_Fo(_Gd,_Ge)):E(_Ga);},_Gf=new T(function(){return B(_mB("Model.hs:98:45-80|lambda"));}),_Gg=function(_Gh){var _Gi=B(_DC(function(_DJ){return new F(function(){return _8i(_Gh,_DJ);});},_Fj));if(!_Gi[0]){return E(_Gf);}else{var _Gj=E(_Gi[1]);if(_Gj[0]==3){var _Gk=E(_Gi[2]);if(!_Gk[0]){return E(_Gf);}else{var _Gl=E(_Gk[1]);return _Gl[0]==3?E(_Gk[2])[0]==0?[3,new T(function(){return B(_Gc(_Gj[1],_Gl[1]));})]:E(_Gf):E(_Gf);}}else{return E(_Gf);}}},_Gm=[7,_Gg,_Fk],_Gn=[0,47],_Go=[1,_Gn,_E],_Gp=[0,_Go,_Gm],_Gq=[1,_Gp,_Ff],_Gr=[1,_Du,_E],_Gs=[1,_Gr,_E],_Gt=[1,_Dx,_E],_Gu=[1,_Gt,_Gs],_Gv=new T(function(){return B(_Dr(_Gu));}),_Gw=new T(function(){return B(_mB("Model.hs:97:45-76|lambda"));}),_Gx=function(_Gy,_Gz){while(1){var _GA=E(_Gy);if(!_GA[0]){var _GB=_GA[1],_GC=E(_Gz);if(!_GC[0]){var _GD=_GC[1],_GE=subC(_GB,_GD);if(!E(_GE[2])){return [0,_GE[1]];}else{_Gy=[1,I_fromInt(_GB)];_Gz=[1,I_fromInt(_GD)];continue;}}else{_Gy=[1,I_fromInt(_GB)];_Gz=_GC;continue;}}else{var _GF=E(_Gz);if(!_GF[0]){_Gy=_GA;_Gz=[1,I_fromInt(_GF[1])];continue;}else{return [1,I_sub(_GA[1],_GF[1])];}}}},_GG=function(_GH){var _GI=B(_DC(function(_DJ){return new F(function(){return _8i(_GH,_DJ);});},_Gu));if(!_GI[0]){return E(_Gw);}else{var _GJ=E(_GI[1]);if(_GJ[0]==3){var _GK=E(_GI[2]);if(!_GK[0]){return E(_Gw);}else{var _GL=E(_GK[1]);return _GL[0]==3?E(_GK[2])[0]==0?[3,new T(function(){return B(_Gx(_GJ[1],_GL[1]));})]:E(_Gw):E(_Gw);}}else{return E(_Gw);}}},_GM=[7,_GG,_Gv],_GN=[0,45],_GO=[1,_GN,_E],_GP=[0,_GO,_GM],_GQ=[1,_GP,_Gq],_GR=[1,_Du,_E],_GS=[1,_GR,_E],_GT=[1,_Dx,_E],_GU=[1,_GT,_GS],_GV=new T(function(){return B(_Dr(_GU));}),_GW=new T(function(){return B(_mB("Model.hs:96:45-76|lambda"));}),_GX=function(_GY){var _GZ=B(_DC(function(_DJ){return new F(function(){return _8i(_GY,_DJ);});},_GU));if(!_GZ[0]){return E(_GW);}else{var _H0=E(_GZ[1]);if(_H0[0]==3){var _H1=E(_GZ[2]);if(!_H1[0]){return E(_GW);}else{var _H2=E(_H1[1]);return _H2[0]==3?E(_H1[2])[0]==0?[3,new T(function(){return B(_oT(_H0[1],_H2[1]));})]:E(_GW):E(_GW);}}else{return E(_GW);}}},_H3=[7,_GX,_GV],_H4=[0,43],_H5=[1,_H4,_E],_H6=[0,_H5,_H3],_H7=[1,_H6,_GQ],_H8=new T(function(){return B(_Dk(_H7));}),_H9=[1,_H8,_E],_Ha=function(_Hb,_){var _Hc=nMV(_H9),_Hd=_Hc,_He=jsSetCB(E(_Hb)[1],E(_BH)[1],function(_Hf,_){if(E(E(_Hf)[1])==13){var _Hg=rMV(_Hd),_Hh=_Hg,_Hi=B(_Bt(E(_Hb)[1],_Hh,_)),_Hj=_Hi,_=wMV(_Hd,_Hj);return _3u;}else{return _3u;}}),_Hk=_He;return _3u;},_Hl=new T(function(){return B(unCStr("repl-input"));}),_Hm=function(_){var _Hn=E(_Hl),_Ho=jsFind(toJSStr(_Hn)),_Hp=_Ho,_Hq=E(_Hp);return _Hq[0]==0?B(_a5(_Hn)):B(_Ha(_Hq[1],_));},_Hr=function(_){return new F(function(){return _Hm(_);});};
var hasteMain = function() {B(A(_Hr, [0]));};window.onload = hasteMain;