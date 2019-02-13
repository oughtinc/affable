(function () {
  'use strict';

  var bind = function bind(fn, thisArg) {
    return function wrap() {
      var args = new Array(arguments.length);
      for (var i = 0; i < args.length; i++) {
        args[i] = arguments[i];
      }
      return fn.apply(thisArg, args);
    };
  };

  /*!
   * Determine if an object is a Buffer
   *
   * @author   Feross Aboukhadijeh <https://feross.org>
   * @license  MIT
   */

  // The _isBuffer check is for Safari 5-7 support, because it's missing
  // Object.prototype.constructor. Remove this eventually
  var isBuffer_1 = function (obj) {
    return obj != null && (isBuffer(obj) || isSlowBuffer(obj) || !!obj._isBuffer)
  };

  function isBuffer (obj) {
    return !!obj.constructor && typeof obj.constructor.isBuffer === 'function' && obj.constructor.isBuffer(obj)
  }

  // For Node v0.10 support. Remove this eventually.
  function isSlowBuffer (obj) {
    return typeof obj.readFloatLE === 'function' && typeof obj.slice === 'function' && isBuffer(obj.slice(0, 0))
  }

  /*global toString:true*/

  // utils is a library of generic helper functions non-specific to axios

  var toString = Object.prototype.toString;

  /**
   * Determine if a value is an Array
   *
   * @param {Object} val The value to test
   * @returns {boolean} True if value is an Array, otherwise false
   */
  function isArray(val) {
    return toString.call(val) === '[object Array]';
  }

  /**
   * Determine if a value is an ArrayBuffer
   *
   * @param {Object} val The value to test
   * @returns {boolean} True if value is an ArrayBuffer, otherwise false
   */
  function isArrayBuffer(val) {
    return toString.call(val) === '[object ArrayBuffer]';
  }

  /**
   * Determine if a value is a FormData
   *
   * @param {Object} val The value to test
   * @returns {boolean} True if value is an FormData, otherwise false
   */
  function isFormData(val) {
    return (typeof FormData !== 'undefined') && (val instanceof FormData);
  }

  /**
   * Determine if a value is a view on an ArrayBuffer
   *
   * @param {Object} val The value to test
   * @returns {boolean} True if value is a view on an ArrayBuffer, otherwise false
   */
  function isArrayBufferView(val) {
    var result;
    if ((typeof ArrayBuffer !== 'undefined') && (ArrayBuffer.isView)) {
      result = ArrayBuffer.isView(val);
    } else {
      result = (val) && (val.buffer) && (val.buffer instanceof ArrayBuffer);
    }
    return result;
  }

  /**
   * Determine if a value is a String
   *
   * @param {Object} val The value to test
   * @returns {boolean} True if value is a String, otherwise false
   */
  function isString(val) {
    return typeof val === 'string';
  }

  /**
   * Determine if a value is a Number
   *
   * @param {Object} val The value to test
   * @returns {boolean} True if value is a Number, otherwise false
   */
  function isNumber(val) {
    return typeof val === 'number';
  }

  /**
   * Determine if a value is undefined
   *
   * @param {Object} val The value to test
   * @returns {boolean} True if the value is undefined, otherwise false
   */
  function isUndefined(val) {
    return typeof val === 'undefined';
  }

  /**
   * Determine if a value is an Object
   *
   * @param {Object} val The value to test
   * @returns {boolean} True if value is an Object, otherwise false
   */
  function isObject(val) {
    return val !== null && typeof val === 'object';
  }

  /**
   * Determine if a value is a Date
   *
   * @param {Object} val The value to test
   * @returns {boolean} True if value is a Date, otherwise false
   */
  function isDate(val) {
    return toString.call(val) === '[object Date]';
  }

  /**
   * Determine if a value is a File
   *
   * @param {Object} val The value to test
   * @returns {boolean} True if value is a File, otherwise false
   */
  function isFile(val) {
    return toString.call(val) === '[object File]';
  }

  /**
   * Determine if a value is a Blob
   *
   * @param {Object} val The value to test
   * @returns {boolean} True if value is a Blob, otherwise false
   */
  function isBlob(val) {
    return toString.call(val) === '[object Blob]';
  }

  /**
   * Determine if a value is a Function
   *
   * @param {Object} val The value to test
   * @returns {boolean} True if value is a Function, otherwise false
   */
  function isFunction(val) {
    return toString.call(val) === '[object Function]';
  }

  /**
   * Determine if a value is a Stream
   *
   * @param {Object} val The value to test
   * @returns {boolean} True if value is a Stream, otherwise false
   */
  function isStream(val) {
    return isObject(val) && isFunction(val.pipe);
  }

  /**
   * Determine if a value is a URLSearchParams object
   *
   * @param {Object} val The value to test
   * @returns {boolean} True if value is a URLSearchParams object, otherwise false
   */
  function isURLSearchParams(val) {
    return typeof URLSearchParams !== 'undefined' && val instanceof URLSearchParams;
  }

  /**
   * Trim excess whitespace off the beginning and end of a string
   *
   * @param {String} str The String to trim
   * @returns {String} The String freed of excess whitespace
   */
  function trim(str) {
    return str.replace(/^\s*/, '').replace(/\s*$/, '');
  }

  /**
   * Determine if we're running in a standard browser environment
   *
   * This allows axios to run in a web worker, and react-native.
   * Both environments support XMLHttpRequest, but not fully standard globals.
   *
   * web workers:
   *  typeof window -> undefined
   *  typeof document -> undefined
   *
   * react-native:
   *  navigator.product -> 'ReactNative'
   */
  function isStandardBrowserEnv() {
    if (typeof navigator !== 'undefined' && navigator.product === 'ReactNative') {
      return false;
    }
    return (
      typeof window !== 'undefined' &&
      typeof document !== 'undefined'
    );
  }

  /**
   * Iterate over an Array or an Object invoking a function for each item.
   *
   * If `obj` is an Array callback will be called passing
   * the value, index, and complete array for each item.
   *
   * If 'obj' is an Object callback will be called passing
   * the value, key, and complete object for each property.
   *
   * @param {Object|Array} obj The object to iterate
   * @param {Function} fn The callback to invoke for each item
   */
  function forEach(obj, fn) {
    // Don't bother if no value provided
    if (obj === null || typeof obj === 'undefined') {
      return;
    }

    // Force an array if not already something iterable
    if (typeof obj !== 'object') {
      /*eslint no-param-reassign:0*/
      obj = [obj];
    }

    if (isArray(obj)) {
      // Iterate over array values
      for (var i = 0, l = obj.length; i < l; i++) {
        fn.call(null, obj[i], i, obj);
      }
    } else {
      // Iterate over object keys
      for (var key in obj) {
        if (Object.prototype.hasOwnProperty.call(obj, key)) {
          fn.call(null, obj[key], key, obj);
        }
      }
    }
  }

  /**
   * Accepts varargs expecting each argument to be an object, then
   * immutably merges the properties of each object and returns result.
   *
   * When multiple objects contain the same key the later object in
   * the arguments list will take precedence.
   *
   * Example:
   *
   * ```js
   * var result = merge({foo: 123}, {foo: 456});
   * console.log(result.foo); // outputs 456
   * ```
   *
   * @param {Object} obj1 Object to merge
   * @returns {Object} Result of all merge properties
   */
  function merge(/* obj1, obj2, obj3, ... */) {
    var result = {};
    function assignValue(val, key) {
      if (typeof result[key] === 'object' && typeof val === 'object') {
        result[key] = merge(result[key], val);
      } else {
        result[key] = val;
      }
    }

    for (var i = 0, l = arguments.length; i < l; i++) {
      forEach(arguments[i], assignValue);
    }
    return result;
  }

  /**
   * Extends object a by mutably adding to it the properties of object b.
   *
   * @param {Object} a The object to be extended
   * @param {Object} b The object to copy properties from
   * @param {Object} thisArg The object to bind function to
   * @return {Object} The resulting value of object a
   */
  function extend(a, b, thisArg) {
    forEach(b, function assignValue(val, key) {
      if (thisArg && typeof val === 'function') {
        a[key] = bind(val, thisArg);
      } else {
        a[key] = val;
      }
    });
    return a;
  }

  var utils = {
    isArray: isArray,
    isArrayBuffer: isArrayBuffer,
    isBuffer: isBuffer_1,
    isFormData: isFormData,
    isArrayBufferView: isArrayBufferView,
    isString: isString,
    isNumber: isNumber,
    isObject: isObject,
    isUndefined: isUndefined,
    isDate: isDate,
    isFile: isFile,
    isBlob: isBlob,
    isFunction: isFunction,
    isStream: isStream,
    isURLSearchParams: isURLSearchParams,
    isStandardBrowserEnv: isStandardBrowserEnv,
    forEach: forEach,
    merge: merge,
    extend: extend,
    trim: trim
  };

  var global$1 = (typeof global !== "undefined" ? global :
              typeof self !== "undefined" ? self :
              typeof window !== "undefined" ? window : {});

  // shim for using process in browser
  // based off https://github.com/defunctzombie/node-process/blob/master/browser.js

  function defaultSetTimout() {
      throw new Error('setTimeout has not been defined');
  }
  function defaultClearTimeout () {
      throw new Error('clearTimeout has not been defined');
  }
  var cachedSetTimeout = defaultSetTimout;
  var cachedClearTimeout = defaultClearTimeout;
  if (typeof global$1.setTimeout === 'function') {
      cachedSetTimeout = setTimeout;
  }
  if (typeof global$1.clearTimeout === 'function') {
      cachedClearTimeout = clearTimeout;
  }

  function runTimeout(fun) {
      if (cachedSetTimeout === setTimeout) {
          //normal enviroments in sane situations
          return setTimeout(fun, 0);
      }
      // if setTimeout wasn't available but was latter defined
      if ((cachedSetTimeout === defaultSetTimout || !cachedSetTimeout) && setTimeout) {
          cachedSetTimeout = setTimeout;
          return setTimeout(fun, 0);
      }
      try {
          // when when somebody has screwed with setTimeout but no I.E. maddness
          return cachedSetTimeout(fun, 0);
      } catch(e){
          try {
              // When we are in I.E. but the script has been evaled so I.E. doesn't trust the global object when called normally
              return cachedSetTimeout.call(null, fun, 0);
          } catch(e){
              // same as above but when it's a version of I.E. that must have the global object for 'this', hopfully our context correct otherwise it will throw a global error
              return cachedSetTimeout.call(this, fun, 0);
          }
      }


  }
  function runClearTimeout(marker) {
      if (cachedClearTimeout === clearTimeout) {
          //normal enviroments in sane situations
          return clearTimeout(marker);
      }
      // if clearTimeout wasn't available but was latter defined
      if ((cachedClearTimeout === defaultClearTimeout || !cachedClearTimeout) && clearTimeout) {
          cachedClearTimeout = clearTimeout;
          return clearTimeout(marker);
      }
      try {
          // when when somebody has screwed with setTimeout but no I.E. maddness
          return cachedClearTimeout(marker);
      } catch (e){
          try {
              // When we are in I.E. but the script has been evaled so I.E. doesn't  trust the global object when called normally
              return cachedClearTimeout.call(null, marker);
          } catch (e){
              // same as above but when it's a version of I.E. that must have the global object for 'this', hopfully our context correct otherwise it will throw a global error.
              // Some versions of I.E. have different rules for clearTimeout vs setTimeout
              return cachedClearTimeout.call(this, marker);
          }
      }



  }
  var queue = [];
  var draining = false;
  var currentQueue;
  var queueIndex = -1;

  function cleanUpNextTick() {
      if (!draining || !currentQueue) {
          return;
      }
      draining = false;
      if (currentQueue.length) {
          queue = currentQueue.concat(queue);
      } else {
          queueIndex = -1;
      }
      if (queue.length) {
          drainQueue();
      }
  }

  function drainQueue() {
      if (draining) {
          return;
      }
      var timeout = runTimeout(cleanUpNextTick);
      draining = true;

      var len = queue.length;
      while(len) {
          currentQueue = queue;
          queue = [];
          while (++queueIndex < len) {
              if (currentQueue) {
                  currentQueue[queueIndex].run();
              }
          }
          queueIndex = -1;
          len = queue.length;
      }
      currentQueue = null;
      draining = false;
      runClearTimeout(timeout);
  }
  function nextTick(fun) {
      var args = new Array(arguments.length - 1);
      if (arguments.length > 1) {
          for (var i = 1; i < arguments.length; i++) {
              args[i - 1] = arguments[i];
          }
      }
      queue.push(new Item(fun, args));
      if (queue.length === 1 && !draining) {
          runTimeout(drainQueue);
      }
  }
  // v8 likes predictible objects
  function Item(fun, array) {
      this.fun = fun;
      this.array = array;
  }
  Item.prototype.run = function () {
      this.fun.apply(null, this.array);
  };
  var title = 'browser';
  var platform = 'browser';
  var browser = true;
  var env = {};
  var argv = [];
  var version = ''; // empty string to avoid regexp issues
  var versions = {};
  var release = {};
  var config = {};

  function noop() {}

  var on = noop;
  var addListener = noop;
  var once = noop;
  var off = noop;
  var removeListener = noop;
  var removeAllListeners = noop;
  var emit = noop;

  function binding(name) {
      throw new Error('process.binding is not supported');
  }

  function cwd () { return '/' }
  function chdir (dir) {
      throw new Error('process.chdir is not supported');
  }function umask() { return 0; }

  // from https://github.com/kumavis/browser-process-hrtime/blob/master/index.js
  var performance = global$1.performance || {};
  var performanceNow =
    performance.now        ||
    performance.mozNow     ||
    performance.msNow      ||
    performance.oNow       ||
    performance.webkitNow  ||
    function(){ return (new Date()).getTime() };

  // generate timestamp or delta
  // see http://nodejs.org/api/process.html#process_process_hrtime
  function hrtime(previousTimestamp){
    var clocktime = performanceNow.call(performance)*1e-3;
    var seconds = Math.floor(clocktime);
    var nanoseconds = Math.floor((clocktime%1)*1e9);
    if (previousTimestamp) {
      seconds = seconds - previousTimestamp[0];
      nanoseconds = nanoseconds - previousTimestamp[1];
      if (nanoseconds<0) {
        seconds--;
        nanoseconds += 1e9;
      }
    }
    return [seconds,nanoseconds]
  }

  var startTime = new Date();
  function uptime() {
    var currentTime = new Date();
    var dif = currentTime - startTime;
    return dif / 1000;
  }

  var process = {
    nextTick: nextTick,
    title: title,
    browser: browser,
    env: env,
    argv: argv,
    version: version,
    versions: versions,
    on: on,
    addListener: addListener,
    once: once,
    off: off,
    removeListener: removeListener,
    removeAllListeners: removeAllListeners,
    emit: emit,
    binding: binding,
    cwd: cwd,
    chdir: chdir,
    umask: umask,
    hrtime: hrtime,
    platform: platform,
    release: release,
    config: config,
    uptime: uptime
  };

  var normalizeHeaderName = function normalizeHeaderName(headers, normalizedName) {
    utils.forEach(headers, function processHeader(value, name) {
      if (name !== normalizedName && name.toUpperCase() === normalizedName.toUpperCase()) {
        headers[normalizedName] = value;
        delete headers[name];
      }
    });
  };

  /**
   * Update an Error with the specified config, error code, and response.
   *
   * @param {Error} error The error to update.
   * @param {Object} config The config.
   * @param {string} [code] The error code (for example, 'ECONNABORTED').
   * @param {Object} [request] The request.
   * @param {Object} [response] The response.
   * @returns {Error} The error.
   */
  var enhanceError = function enhanceError(error, config, code, request, response) {
    error.config = config;
    if (code) {
      error.code = code;
    }
    error.request = request;
    error.response = response;
    return error;
  };

  /**
   * Create an Error with the specified message, config, error code, request and response.
   *
   * @param {string} message The error message.
   * @param {Object} config The config.
   * @param {string} [code] The error code (for example, 'ECONNABORTED').
   * @param {Object} [request] The request.
   * @param {Object} [response] The response.
   * @returns {Error} The created error.
   */
  var createError = function createError(message, config, code, request, response) {
    var error = new Error(message);
    return enhanceError(error, config, code, request, response);
  };

  /**
   * Resolve or reject a Promise based on response status.
   *
   * @param {Function} resolve A function that resolves the promise.
   * @param {Function} reject A function that rejects the promise.
   * @param {object} response The response.
   */
  var settle = function settle(resolve, reject, response) {
    var validateStatus = response.config.validateStatus;
    // Note: status is not exposed by XDomainRequest
    if (!response.status || !validateStatus || validateStatus(response.status)) {
      resolve(response);
    } else {
      reject(createError(
        'Request failed with status code ' + response.status,
        response.config,
        null,
        response.request,
        response
      ));
    }
  };

  function encode(val) {
    return encodeURIComponent(val).
      replace(/%40/gi, '@').
      replace(/%3A/gi, ':').
      replace(/%24/g, '$').
      replace(/%2C/gi, ',').
      replace(/%20/g, '+').
      replace(/%5B/gi, '[').
      replace(/%5D/gi, ']');
  }

  /**
   * Build a URL by appending params to the end
   *
   * @param {string} url The base of the url (e.g., http://www.google.com)
   * @param {object} [params] The params to be appended
   * @returns {string} The formatted url
   */
  var buildURL = function buildURL(url, params, paramsSerializer) {
    /*eslint no-param-reassign:0*/
    if (!params) {
      return url;
    }

    var serializedParams;
    if (paramsSerializer) {
      serializedParams = paramsSerializer(params);
    } else if (utils.isURLSearchParams(params)) {
      serializedParams = params.toString();
    } else {
      var parts = [];

      utils.forEach(params, function serialize(val, key) {
        if (val === null || typeof val === 'undefined') {
          return;
        }

        if (utils.isArray(val)) {
          key = key + '[]';
        } else {
          val = [val];
        }

        utils.forEach(val, function parseValue(v) {
          if (utils.isDate(v)) {
            v = v.toISOString();
          } else if (utils.isObject(v)) {
            v = JSON.stringify(v);
          }
          parts.push(encode(key) + '=' + encode(v));
        });
      });

      serializedParams = parts.join('&');
    }

    if (serializedParams) {
      url += (url.indexOf('?') === -1 ? '?' : '&') + serializedParams;
    }

    return url;
  };

  // Headers whose duplicates are ignored by node
  // c.f. https://nodejs.org/api/http.html#http_message_headers
  var ignoreDuplicateOf = [
    'age', 'authorization', 'content-length', 'content-type', 'etag',
    'expires', 'from', 'host', 'if-modified-since', 'if-unmodified-since',
    'last-modified', 'location', 'max-forwards', 'proxy-authorization',
    'referer', 'retry-after', 'user-agent'
  ];

  /**
   * Parse headers into an object
   *
   * ```
   * Date: Wed, 27 Aug 2014 08:58:49 GMT
   * Content-Type: application/json
   * Connection: keep-alive
   * Transfer-Encoding: chunked
   * ```
   *
   * @param {String} headers Headers needing to be parsed
   * @returns {Object} Headers parsed into an object
   */
  var parseHeaders = function parseHeaders(headers) {
    var parsed = {};
    var key;
    var val;
    var i;

    if (!headers) { return parsed; }

    utils.forEach(headers.split('\n'), function parser(line) {
      i = line.indexOf(':');
      key = utils.trim(line.substr(0, i)).toLowerCase();
      val = utils.trim(line.substr(i + 1));

      if (key) {
        if (parsed[key] && ignoreDuplicateOf.indexOf(key) >= 0) {
          return;
        }
        if (key === 'set-cookie') {
          parsed[key] = (parsed[key] ? parsed[key] : []).concat([val]);
        } else {
          parsed[key] = parsed[key] ? parsed[key] + ', ' + val : val;
        }
      }
    });

    return parsed;
  };

  var isURLSameOrigin = (
    utils.isStandardBrowserEnv() ?

    // Standard browser envs have full support of the APIs needed to test
    // whether the request URL is of the same origin as current location.
    (function standardBrowserEnv() {
      var msie = /(msie|trident)/i.test(navigator.userAgent);
      var urlParsingNode = document.createElement('a');
      var originURL;

      /**
      * Parse a URL to discover it's components
      *
      * @param {String} url The URL to be parsed
      * @returns {Object}
      */
      function resolveURL(url) {
        var href = url;

        if (msie) {
          // IE needs attribute set twice to normalize properties
          urlParsingNode.setAttribute('href', href);
          href = urlParsingNode.href;
        }

        urlParsingNode.setAttribute('href', href);

        // urlParsingNode provides the UrlUtils interface - http://url.spec.whatwg.org/#urlutils
        return {
          href: urlParsingNode.href,
          protocol: urlParsingNode.protocol ? urlParsingNode.protocol.replace(/:$/, '') : '',
          host: urlParsingNode.host,
          search: urlParsingNode.search ? urlParsingNode.search.replace(/^\?/, '') : '',
          hash: urlParsingNode.hash ? urlParsingNode.hash.replace(/^#/, '') : '',
          hostname: urlParsingNode.hostname,
          port: urlParsingNode.port,
          pathname: (urlParsingNode.pathname.charAt(0) === '/') ?
                    urlParsingNode.pathname :
                    '/' + urlParsingNode.pathname
        };
      }

      originURL = resolveURL(window.location.href);

      /**
      * Determine if a URL shares the same origin as the current location
      *
      * @param {String} requestURL The URL to test
      * @returns {boolean} True if URL shares the same origin, otherwise false
      */
      return function isURLSameOrigin(requestURL) {
        var parsed = (utils.isString(requestURL)) ? resolveURL(requestURL) : requestURL;
        return (parsed.protocol === originURL.protocol &&
              parsed.host === originURL.host);
      };
    })() :

    // Non standard browser envs (web workers, react-native) lack needed support.
    (function nonStandardBrowserEnv() {
      return function isURLSameOrigin() {
        return true;
      };
    })()
  );

  // btoa polyfill for IE<10 courtesy https://github.com/davidchambers/Base64.js

  var chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=';

  function E() {
    this.message = 'String contains an invalid character';
  }
  E.prototype = new Error;
  E.prototype.code = 5;
  E.prototype.name = 'InvalidCharacterError';

  function btoa(input) {
    var str = String(input);
    var output = '';
    for (
      // initialize result and counter
      var block, charCode, idx = 0, map = chars;
      // if the next str index does not exist:
      //   change the mapping table to "="
      //   check if d has no fractional digits
      str.charAt(idx | 0) || (map = '=', idx % 1);
      // "8 - idx % 1 * 8" generates the sequence 2, 4, 6, 8
      output += map.charAt(63 & block >> 8 - idx % 1 * 8)
    ) {
      charCode = str.charCodeAt(idx += 3 / 4);
      if (charCode > 0xFF) {
        throw new E();
      }
      block = block << 8 | charCode;
    }
    return output;
  }

  var btoa_1 = btoa;

  var cookies = (
    utils.isStandardBrowserEnv() ?

    // Standard browser envs support document.cookie
    (function standardBrowserEnv() {
      return {
        write: function write(name, value, expires, path, domain, secure) {
          var cookie = [];
          cookie.push(name + '=' + encodeURIComponent(value));

          if (utils.isNumber(expires)) {
            cookie.push('expires=' + new Date(expires).toGMTString());
          }

          if (utils.isString(path)) {
            cookie.push('path=' + path);
          }

          if (utils.isString(domain)) {
            cookie.push('domain=' + domain);
          }

          if (secure === true) {
            cookie.push('secure');
          }

          document.cookie = cookie.join('; ');
        },

        read: function read(name) {
          var match = document.cookie.match(new RegExp('(^|;\\s*)(' + name + ')=([^;]*)'));
          return (match ? decodeURIComponent(match[3]) : null);
        },

        remove: function remove(name) {
          this.write(name, '', Date.now() - 86400000);
        }
      };
    })() :

    // Non standard browser env (web workers, react-native) lack needed support.
    (function nonStandardBrowserEnv() {
      return {
        write: function write() {},
        read: function read() { return null; },
        remove: function remove() {}
      };
    })()
  );

  var btoa$1 = (typeof window !== 'undefined' && window.btoa && window.btoa.bind(window)) || btoa_1;

  var xhr = function xhrAdapter(config$$1) {
    return new Promise(function dispatchXhrRequest(resolve, reject) {
      var requestData = config$$1.data;
      var requestHeaders = config$$1.headers;

      if (utils.isFormData(requestData)) {
        delete requestHeaders['Content-Type']; // Let the browser set it
      }

      var request = new XMLHttpRequest();
      var loadEvent = 'onreadystatechange';
      var xDomain = false;

      // For IE 8/9 CORS support
      // Only supports POST and GET calls and doesn't returns the response headers.
      // DON'T do this for testing b/c XMLHttpRequest is mocked, not XDomainRequest.
      if (process.env.NODE_ENV !== 'test' &&
          typeof window !== 'undefined' &&
          window.XDomainRequest && !('withCredentials' in request) &&
          !isURLSameOrigin(config$$1.url)) {
        request = new window.XDomainRequest();
        loadEvent = 'onload';
        xDomain = true;
        request.onprogress = function handleProgress() {};
        request.ontimeout = function handleTimeout() {};
      }

      // HTTP basic authentication
      if (config$$1.auth) {
        var username = config$$1.auth.username || '';
        var password = config$$1.auth.password || '';
        requestHeaders.Authorization = 'Basic ' + btoa$1(username + ':' + password);
      }

      request.open(config$$1.method.toUpperCase(), buildURL(config$$1.url, config$$1.params, config$$1.paramsSerializer), true);

      // Set the request timeout in MS
      request.timeout = config$$1.timeout;

      // Listen for ready state
      request[loadEvent] = function handleLoad() {
        if (!request || (request.readyState !== 4 && !xDomain)) {
          return;
        }

        // The request errored out and we didn't get a response, this will be
        // handled by onerror instead
        // With one exception: request that using file: protocol, most browsers
        // will return status as 0 even though it's a successful request
        if (request.status === 0 && !(request.responseURL && request.responseURL.indexOf('file:') === 0)) {
          return;
        }

        // Prepare the response
        var responseHeaders = 'getAllResponseHeaders' in request ? parseHeaders(request.getAllResponseHeaders()) : null;
        var responseData = !config$$1.responseType || config$$1.responseType === 'text' ? request.responseText : request.response;
        var response = {
          data: responseData,
          // IE sends 1223 instead of 204 (https://github.com/axios/axios/issues/201)
          status: request.status === 1223 ? 204 : request.status,
          statusText: request.status === 1223 ? 'No Content' : request.statusText,
          headers: responseHeaders,
          config: config$$1,
          request: request
        };

        settle(resolve, reject, response);

        // Clean up request
        request = null;
      };

      // Handle low level network errors
      request.onerror = function handleError() {
        // Real errors are hidden from us by the browser
        // onerror should only fire if it's a network error
        reject(createError('Network Error', config$$1, null, request));

        // Clean up request
        request = null;
      };

      // Handle timeout
      request.ontimeout = function handleTimeout() {
        reject(createError('timeout of ' + config$$1.timeout + 'ms exceeded', config$$1, 'ECONNABORTED',
          request));

        // Clean up request
        request = null;
      };

      // Add xsrf header
      // This is only done if running in a standard browser environment.
      // Specifically not if we're in a web worker, or react-native.
      if (utils.isStandardBrowserEnv()) {
        var cookies$$1 = cookies;

        // Add xsrf header
        var xsrfValue = (config$$1.withCredentials || isURLSameOrigin(config$$1.url)) && config$$1.xsrfCookieName ?
            cookies$$1.read(config$$1.xsrfCookieName) :
            undefined;

        if (xsrfValue) {
          requestHeaders[config$$1.xsrfHeaderName] = xsrfValue;
        }
      }

      // Add headers to the request
      if ('setRequestHeader' in request) {
        utils.forEach(requestHeaders, function setRequestHeader(val, key) {
          if (typeof requestData === 'undefined' && key.toLowerCase() === 'content-type') {
            // Remove Content-Type if data is undefined
            delete requestHeaders[key];
          } else {
            // Otherwise add header to the request
            request.setRequestHeader(key, val);
          }
        });
      }

      // Add withCredentials to request if needed
      if (config$$1.withCredentials) {
        request.withCredentials = true;
      }

      // Add responseType to request if needed
      if (config$$1.responseType) {
        try {
          request.responseType = config$$1.responseType;
        } catch (e) {
          // Expected DOMException thrown by browsers not compatible XMLHttpRequest Level 2.
          // But, this can be suppressed for 'json' type as it can be parsed by default 'transformResponse' function.
          if (config$$1.responseType !== 'json') {
            throw e;
          }
        }
      }

      // Handle progress if needed
      if (typeof config$$1.onDownloadProgress === 'function') {
        request.addEventListener('progress', config$$1.onDownloadProgress);
      }

      // Not all browsers support upload events
      if (typeof config$$1.onUploadProgress === 'function' && request.upload) {
        request.upload.addEventListener('progress', config$$1.onUploadProgress);
      }

      if (config$$1.cancelToken) {
        // Handle cancellation
        config$$1.cancelToken.promise.then(function onCanceled(cancel) {
          if (!request) {
            return;
          }

          request.abort();
          reject(cancel);
          // Clean up request
          request = null;
        });
      }

      if (requestData === undefined) {
        requestData = null;
      }

      // Send the request
      request.send(requestData);
    });
  };

  var DEFAULT_CONTENT_TYPE = {
    'Content-Type': 'application/x-www-form-urlencoded'
  };

  function setContentTypeIfUnset(headers, value) {
    if (!utils.isUndefined(headers) && utils.isUndefined(headers['Content-Type'])) {
      headers['Content-Type'] = value;
    }
  }

  function getDefaultAdapter() {
    var adapter;
    if (typeof XMLHttpRequest !== 'undefined') {
      // For browsers use XHR adapter
      adapter = xhr;
    } else if (typeof process !== 'undefined') {
      // For node use HTTP adapter
      adapter = xhr;
    }
    return adapter;
  }

  var defaults = {
    adapter: getDefaultAdapter(),

    transformRequest: [function transformRequest(data, headers) {
      normalizeHeaderName(headers, 'Content-Type');
      if (utils.isFormData(data) ||
        utils.isArrayBuffer(data) ||
        utils.isBuffer(data) ||
        utils.isStream(data) ||
        utils.isFile(data) ||
        utils.isBlob(data)
      ) {
        return data;
      }
      if (utils.isArrayBufferView(data)) {
        return data.buffer;
      }
      if (utils.isURLSearchParams(data)) {
        setContentTypeIfUnset(headers, 'application/x-www-form-urlencoded;charset=utf-8');
        return data.toString();
      }
      if (utils.isObject(data)) {
        setContentTypeIfUnset(headers, 'application/json;charset=utf-8');
        return JSON.stringify(data);
      }
      return data;
    }],

    transformResponse: [function transformResponse(data) {
      /*eslint no-param-reassign:0*/
      if (typeof data === 'string') {
        try {
          data = JSON.parse(data);
        } catch (e) { /* Ignore */ }
      }
      return data;
    }],

    /**
     * A timeout in milliseconds to abort a request. If set to 0 (default) a
     * timeout is not created.
     */
    timeout: 0,

    xsrfCookieName: 'XSRF-TOKEN',
    xsrfHeaderName: 'X-XSRF-TOKEN',

    maxContentLength: -1,

    validateStatus: function validateStatus(status) {
      return status >= 200 && status < 300;
    }
  };

  defaults.headers = {
    common: {
      'Accept': 'application/json, text/plain, */*'
    }
  };

  utils.forEach(['delete', 'get', 'head'], function forEachMethodNoData(method) {
    defaults.headers[method] = {};
  });

  utils.forEach(['post', 'put', 'patch'], function forEachMethodWithData(method) {
    defaults.headers[method] = utils.merge(DEFAULT_CONTENT_TYPE);
  });

  var defaults_1 = defaults;

  function InterceptorManager() {
    this.handlers = [];
  }

  /**
   * Add a new interceptor to the stack
   *
   * @param {Function} fulfilled The function to handle `then` for a `Promise`
   * @param {Function} rejected The function to handle `reject` for a `Promise`
   *
   * @return {Number} An ID used to remove interceptor later
   */
  InterceptorManager.prototype.use = function use(fulfilled, rejected) {
    this.handlers.push({
      fulfilled: fulfilled,
      rejected: rejected
    });
    return this.handlers.length - 1;
  };

  /**
   * Remove an interceptor from the stack
   *
   * @param {Number} id The ID that was returned by `use`
   */
  InterceptorManager.prototype.eject = function eject(id) {
    if (this.handlers[id]) {
      this.handlers[id] = null;
    }
  };

  /**
   * Iterate over all the registered interceptors
   *
   * This method is particularly useful for skipping over any
   * interceptors that may have become `null` calling `eject`.
   *
   * @param {Function} fn The function to call for each interceptor
   */
  InterceptorManager.prototype.forEach = function forEach(fn) {
    utils.forEach(this.handlers, function forEachHandler(h) {
      if (h !== null) {
        fn(h);
      }
    });
  };

  var InterceptorManager_1 = InterceptorManager;

  /**
   * Transform the data for a request or a response
   *
   * @param {Object|String} data The data to be transformed
   * @param {Array} headers The headers for the request or response
   * @param {Array|Function} fns A single function or Array of functions
   * @returns {*} The resulting transformed data
   */
  var transformData = function transformData(data, headers, fns) {
    /*eslint no-param-reassign:0*/
    utils.forEach(fns, function transform(fn) {
      data = fn(data, headers);
    });

    return data;
  };

  var isCancel = function isCancel(value) {
    return !!(value && value.__CANCEL__);
  };

  /**
   * Determines whether the specified URL is absolute
   *
   * @param {string} url The URL to test
   * @returns {boolean} True if the specified URL is absolute, otherwise false
   */
  var isAbsoluteURL = function isAbsoluteURL(url) {
    // A URL is considered absolute if it begins with "<scheme>://" or "//" (protocol-relative URL).
    // RFC 3986 defines scheme name as a sequence of characters beginning with a letter and followed
    // by any combination of letters, digits, plus, period, or hyphen.
    return /^([a-z][a-z\d\+\-\.]*:)?\/\//i.test(url);
  };

  /**
   * Creates a new URL by combining the specified URLs
   *
   * @param {string} baseURL The base URL
   * @param {string} relativeURL The relative URL
   * @returns {string} The combined URL
   */
  var combineURLs = function combineURLs(baseURL, relativeURL) {
    return relativeURL
      ? baseURL.replace(/\/+$/, '') + '/' + relativeURL.replace(/^\/+/, '')
      : baseURL;
  };

  /**
   * Throws a `Cancel` if cancellation has been requested.
   */
  function throwIfCancellationRequested(config) {
    if (config.cancelToken) {
      config.cancelToken.throwIfRequested();
    }
  }

  /**
   * Dispatch a request to the server using the configured adapter.
   *
   * @param {object} config The config that is to be used for the request
   * @returns {Promise} The Promise to be fulfilled
   */
  var dispatchRequest = function dispatchRequest(config) {
    throwIfCancellationRequested(config);

    // Support baseURL config
    if (config.baseURL && !isAbsoluteURL(config.url)) {
      config.url = combineURLs(config.baseURL, config.url);
    }

    // Ensure headers exist
    config.headers = config.headers || {};

    // Transform request data
    config.data = transformData(
      config.data,
      config.headers,
      config.transformRequest
    );

    // Flatten headers
    config.headers = utils.merge(
      config.headers.common || {},
      config.headers[config.method] || {},
      config.headers || {}
    );

    utils.forEach(
      ['delete', 'get', 'head', 'post', 'put', 'patch', 'common'],
      function cleanHeaderConfig(method) {
        delete config.headers[method];
      }
    );

    var adapter = config.adapter || defaults_1.adapter;

    return adapter(config).then(function onAdapterResolution(response) {
      throwIfCancellationRequested(config);

      // Transform response data
      response.data = transformData(
        response.data,
        response.headers,
        config.transformResponse
      );

      return response;
    }, function onAdapterRejection(reason) {
      if (!isCancel(reason)) {
        throwIfCancellationRequested(config);

        // Transform response data
        if (reason && reason.response) {
          reason.response.data = transformData(
            reason.response.data,
            reason.response.headers,
            config.transformResponse
          );
        }
      }

      return Promise.reject(reason);
    });
  };

  /**
   * Create a new instance of Axios
   *
   * @param {Object} instanceConfig The default config for the instance
   */
  function Axios(instanceConfig) {
    this.defaults = instanceConfig;
    this.interceptors = {
      request: new InterceptorManager_1(),
      response: new InterceptorManager_1()
    };
  }

  /**
   * Dispatch a request
   *
   * @param {Object} config The config specific for this request (merged with this.defaults)
   */
  Axios.prototype.request = function request(config) {
    /*eslint no-param-reassign:0*/
    // Allow for axios('example/url'[, config]) a la fetch API
    if (typeof config === 'string') {
      config = utils.merge({
        url: arguments[0]
      }, arguments[1]);
    }

    config = utils.merge(defaults_1, {method: 'get'}, this.defaults, config);
    config.method = config.method.toLowerCase();

    // Hook up interceptors middleware
    var chain = [dispatchRequest, undefined];
    var promise = Promise.resolve(config);

    this.interceptors.request.forEach(function unshiftRequestInterceptors(interceptor) {
      chain.unshift(interceptor.fulfilled, interceptor.rejected);
    });

    this.interceptors.response.forEach(function pushResponseInterceptors(interceptor) {
      chain.push(interceptor.fulfilled, interceptor.rejected);
    });

    while (chain.length) {
      promise = promise.then(chain.shift(), chain.shift());
    }

    return promise;
  };

  // Provide aliases for supported request methods
  utils.forEach(['delete', 'get', 'head', 'options'], function forEachMethodNoData(method) {
    /*eslint func-names:0*/
    Axios.prototype[method] = function(url, config) {
      return this.request(utils.merge(config || {}, {
        method: method,
        url: url
      }));
    };
  });

  utils.forEach(['post', 'put', 'patch'], function forEachMethodWithData(method) {
    /*eslint func-names:0*/
    Axios.prototype[method] = function(url, data, config) {
      return this.request(utils.merge(config || {}, {
        method: method,
        url: url,
        data: data
      }));
    };
  });

  var Axios_1 = Axios;

  /**
   * A `Cancel` is an object that is thrown when an operation is canceled.
   *
   * @class
   * @param {string=} message The message.
   */
  function Cancel(message) {
    this.message = message;
  }

  Cancel.prototype.toString = function toString() {
    return 'Cancel' + (this.message ? ': ' + this.message : '');
  };

  Cancel.prototype.__CANCEL__ = true;

  var Cancel_1 = Cancel;

  /**
   * A `CancelToken` is an object that can be used to request cancellation of an operation.
   *
   * @class
   * @param {Function} executor The executor function.
   */
  function CancelToken(executor) {
    if (typeof executor !== 'function') {
      throw new TypeError('executor must be a function.');
    }

    var resolvePromise;
    this.promise = new Promise(function promiseExecutor(resolve) {
      resolvePromise = resolve;
    });

    var token = this;
    executor(function cancel(message) {
      if (token.reason) {
        // Cancellation has already been requested
        return;
      }

      token.reason = new Cancel_1(message);
      resolvePromise(token.reason);
    });
  }

  /**
   * Throws a `Cancel` if cancellation has been requested.
   */
  CancelToken.prototype.throwIfRequested = function throwIfRequested() {
    if (this.reason) {
      throw this.reason;
    }
  };

  /**
   * Returns an object that contains a new `CancelToken` and a function that, when called,
   * cancels the `CancelToken`.
   */
  CancelToken.source = function source() {
    var cancel;
    var token = new CancelToken(function executor(c) {
      cancel = c;
    });
    return {
      token: token,
      cancel: cancel
    };
  };

  var CancelToken_1 = CancelToken;

  /**
   * Syntactic sugar for invoking a function and expanding an array for arguments.
   *
   * Common use case would be to use `Function.prototype.apply`.
   *
   *  ```js
   *  function f(x, y, z) {}
   *  var args = [1, 2, 3];
   *  f.apply(null, args);
   *  ```
   *
   * With `spread` this example can be re-written.
   *
   *  ```js
   *  spread(function(x, y, z) {})([1, 2, 3]);
   *  ```
   *
   * @param {Function} callback
   * @returns {Function}
   */
  var spread = function spread(callback) {
    return function wrap(arr) {
      return callback.apply(null, arr);
    };
  };

  /**
   * Create an instance of Axios
   *
   * @param {Object} defaultConfig The default config for the instance
   * @return {Axios} A new instance of Axios
   */
  function createInstance(defaultConfig) {
    var context = new Axios_1(defaultConfig);
    var instance = bind(Axios_1.prototype.request, context);

    // Copy axios.prototype to instance
    utils.extend(instance, Axios_1.prototype, context);

    // Copy context to instance
    utils.extend(instance, context);

    return instance;
  }

  // Create the default instance to be exported
  var axios = createInstance(defaults_1);

  // Expose Axios class to allow class inheritance
  axios.Axios = Axios_1;

  // Factory for creating new instances
  axios.create = function create(instanceConfig) {
    return createInstance(utils.merge(defaults_1, instanceConfig));
  };

  // Expose Cancel & CancelToken
  axios.Cancel = Cancel_1;
  axios.CancelToken = CancelToken_1;
  axios.isCancel = isCancel;

  // Expose all/spread
  axios.all = function all(promises) {
    return Promise.all(promises);
  };
  axios.spread = spread;

  var axios_1 = axios;

  // Allow use of default import syntax in TypeScript
  var default_1 = axios;
  axios_1.default = default_1;

  var axios$1 = axios_1;

  function postReply(body) {
      return axios$1({ url: '/reply',
          method: 'post',
          data: body,
          responseType: 'json'
      });
  }
  function postWait(body) {
      return axios$1({ url: '/wait',
          method: 'post',
          data: body,
          responseType: 'json'
      });
  }
  function postNext(body) {
      return axios$1({ url: '/next',
          method: 'post',
          data: body,
          responseType: 'json'
      });
  }
  function getJoin() {
      return axios$1({ url: '/join',
          method: 'get'
      });
  }
  function getPointer(p) {
      return axios$1({ url: '/pointer/' + encodeURIComponent('' + p) + '',
          method: 'get'
      });
  }

  /*
   * Generated by PEG.js 0.10.0.
   *
   * http://pegjs.org/
   */
  /** @type {(input: string, options?: any) => any */
  const messageParser = (function() {
    function peg$subclass(child, parent) {
      function ctor() { this.constructor = child; }
      ctor.prototype = parent.prototype;
      child.prototype = new ctor();
    }

    function peg$SyntaxError(message, expected, found, location) {
      this.message  = message;
      this.expected = expected;
      this.found    = found;
      this.location = location;
      this.name     = "SyntaxError";

      if (typeof Error.captureStackTrace === "function") {
        Error.captureStackTrace(this, peg$SyntaxError);
      }
    }

    peg$subclass(peg$SyntaxError, Error);

    peg$SyntaxError.buildMessage = function(expected, found) {
      var DESCRIBE_EXPECTATION_FNS = {
            literal: function(expectation) {
              return "\"" + literalEscape(expectation.text) + "\"";
            },

            "class": function(expectation) {
              var escapedParts = "",
                  i;

              for (i = 0; i < expectation.parts.length; i++) {
                escapedParts += expectation.parts[i] instanceof Array
                  ? classEscape(expectation.parts[i][0]) + "-" + classEscape(expectation.parts[i][1])
                  : classEscape(expectation.parts[i]);
              }

              return "[" + (expectation.inverted ? "^" : "") + escapedParts + "]";
            },

            any: function(expectation) {
              return "any character";
            },

            end: function(expectation) {
              return "end of input";
            },

            other: function(expectation) {
              return expectation.description;
            }
          };

      function hex(ch) {
        return ch.charCodeAt(0).toString(16).toUpperCase();
      }

      function literalEscape(s) {
        return s
          .replace(/\\/g, '\\\\')
          .replace(/"/g,  '\\"')
          .replace(/\0/g, '\\0')
          .replace(/\t/g, '\\t')
          .replace(/\n/g, '\\n')
          .replace(/\r/g, '\\r')
          .replace(/[\x00-\x0F]/g,          function(ch) { return '\\x0' + hex(ch); })
          .replace(/[\x10-\x1F\x7F-\x9F]/g, function(ch) { return '\\x'  + hex(ch); });
      }

      function classEscape(s) {
        return s
          .replace(/\\/g, '\\\\')
          .replace(/\]/g, '\\]')
          .replace(/\^/g, '\\^')
          .replace(/-/g,  '\\-')
          .replace(/\0/g, '\\0')
          .replace(/\t/g, '\\t')
          .replace(/\n/g, '\\n')
          .replace(/\r/g, '\\r')
          .replace(/[\x00-\x0F]/g,          function(ch) { return '\\x0' + hex(ch); })
          .replace(/[\x10-\x1F\x7F-\x9F]/g, function(ch) { return '\\x'  + hex(ch); });
      }

      function describeExpectation(expectation) {
        return DESCRIBE_EXPECTATION_FNS[expectation.type](expectation);
      }

      function describeExpected(expected) {
        var descriptions = new Array(expected.length),
            i, j;

        for (i = 0; i < expected.length; i++) {
          descriptions[i] = describeExpectation(expected[i]);
        }

        descriptions.sort();

        if (descriptions.length > 0) {
          for (i = 1, j = 1; i < descriptions.length; i++) {
            if (descriptions[i - 1] !== descriptions[i]) {
              descriptions[j] = descriptions[i];
              j++;
            }
          }
          descriptions.length = j;
        }

        switch (descriptions.length) {
          case 1:
            return descriptions[0];

          case 2:
            return descriptions[0] + " or " + descriptions[1];

          default:
            return descriptions.slice(0, -1).join(", ")
              + ", or "
              + descriptions[descriptions.length - 1];
        }
      }

      function describeFound(found) {
        return found ? "\"" + literalEscape(found) + "\"" : "end of input";
      }

      return "Expected " + describeExpected(expected) + " but " + describeFound(found) + " found.";
    };

    function peg$parse(input, options) {
      options = options !== void 0 ? options : {};

      var peg$FAILED = {},

          peg$startRuleIndices = { Top: 0 },
          peg$startRuleIndex   = 0,

          peg$consts = [
            peg$otherExpectation("message"),
            function(msgs) { return {tag: 'Structured', contents: msgs}; },
            peg$otherExpectation("submessage"),
            /^[^\][$]/,
            peg$classExpectation(["]", "[", "$"], true, false),
            function() { return {tag: 'Text', contents: text()}; },
            "[",
            peg$literalExpectation("[", false),
            "]",
            peg$literalExpectation("]", false),
            peg$otherExpectation("pointer"),
            "$",
            peg$literalExpectation("$", false),
            /^[0-9]/,
            peg$classExpectation([["0", "9"]], false, false),
            function(digits) { return {tag: 'Reference', contents: parseInt(digits.join(''), 10)}; }
          ],

          peg$bytecode = [
            peg$decode("<%$;!/&#0#*;!&&&#/' 8!:!!! )=.\" 7 "),
            peg$decode("<;\".\x87 &%$4#\"\"5!7$/,#0)*4#\"\"5!7$&&&#/& 8!:%! ).] &%2&\"\"6&7'/M#$;!/&#0#*;!&&&#/7$2(\"\"6(7)/($8#:!#!!)(#'#(\"'#&'#=.\" 7\""),
            peg$decode("<%2+\"\"6+7,/J#$4-\"\"5!7./,#0)*4-\"\"5!7.&&&#/($8\":/\"! )(\"'#&'#=.\" 7*")
          ],

          peg$currPos          = 0,
          peg$savedPos         = 0,
          peg$posDetailsCache  = [{ line: 1, column: 1 }],
          peg$maxFailPos       = 0,
          peg$maxFailExpected  = [],
          peg$silentFails      = 0,

          peg$result;

      if ("startRule" in options) {
        if (!(options.startRule in peg$startRuleIndices)) {
          throw new Error("Can't start parsing from rule \"" + options.startRule + "\".");
        }

        peg$startRuleIndex = peg$startRuleIndices[options.startRule];
      }

      function text() {
        return input.substring(peg$savedPos, peg$currPos);
      }

      function peg$literalExpectation(text, ignoreCase) {
        return { type: "literal", text: text, ignoreCase: ignoreCase };
      }

      function peg$classExpectation(parts, inverted, ignoreCase) {
        return { type: "class", parts: parts, inverted: inverted, ignoreCase: ignoreCase };
      }

      function peg$endExpectation() {
        return { type: "end" };
      }

      function peg$otherExpectation(description) {
        return { type: "other", description: description };
      }

      function peg$computePosDetails(pos) {
        var details = peg$posDetailsCache[pos], p;

        if (details) {
          return details;
        } else {
          p = pos - 1;
          while (!peg$posDetailsCache[p]) {
            p--;
          }

          details = peg$posDetailsCache[p];
          details = {
            line:   details.line,
            column: details.column
          };

          while (p < pos) {
            if (input.charCodeAt(p) === 10) {
              details.line++;
              details.column = 1;
            } else {
              details.column++;
            }

            p++;
          }

          peg$posDetailsCache[pos] = details;
          return details;
        }
      }

      function peg$computeLocation(startPos, endPos) {
        var startPosDetails = peg$computePosDetails(startPos),
            endPosDetails   = peg$computePosDetails(endPos);

        return {
          start: {
            offset: startPos,
            line:   startPosDetails.line,
            column: startPosDetails.column
          },
          end: {
            offset: endPos,
            line:   endPosDetails.line,
            column: endPosDetails.column
          }
        };
      }

      function peg$fail(expected) {
        if (peg$currPos < peg$maxFailPos) { return; }

        if (peg$currPos > peg$maxFailPos) {
          peg$maxFailPos = peg$currPos;
          peg$maxFailExpected = [];
        }

        peg$maxFailExpected.push(expected);
      }

      function peg$buildStructuredError(expected, found, location) {
        return new peg$SyntaxError(
          peg$SyntaxError.buildMessage(expected, found),
          expected,
          found,
          location
        );
      }

      function peg$decode(s) {
        var bc = new Array(s.length), i;

        for (i = 0; i < s.length; i++) {
          bc[i] = s.charCodeAt(i) - 32;
        }

        return bc;
      }

      function peg$parseRule(index) {
        var bc    = peg$bytecode[index],
            ip    = 0,
            ips   = [],
            end   = bc.length,
            ends  = [],
            stack = [],
            params, i;

        while (true) {
          while (ip < end) {
            switch (bc[ip]) {
              case 0:
                stack.push(peg$consts[bc[ip + 1]]);
                ip += 2;
                break;

              case 1:
                stack.push(void 0);
                ip++;
                break;

              case 2:
                stack.push(null);
                ip++;
                break;

              case 3:
                stack.push(peg$FAILED);
                ip++;
                break;

              case 4:
                stack.push([]);
                ip++;
                break;

              case 5:
                stack.push(peg$currPos);
                ip++;
                break;

              case 6:
                stack.pop();
                ip++;
                break;

              case 7:
                peg$currPos = stack.pop();
                ip++;
                break;

              case 8:
                stack.length -= bc[ip + 1];
                ip += 2;
                break;

              case 9:
                stack.splice(-2, 1);
                ip++;
                break;

              case 10:
                stack[stack.length - 2].push(stack.pop());
                ip++;
                break;

              case 11:
                stack.push(stack.splice(stack.length - bc[ip + 1], bc[ip + 1]));
                ip += 2;
                break;

              case 12:
                stack.push(input.substring(stack.pop(), peg$currPos));
                ip++;
                break;

              case 13:
                ends.push(end);
                ips.push(ip + 3 + bc[ip + 1] + bc[ip + 2]);

                if (stack[stack.length - 1]) {
                  end = ip + 3 + bc[ip + 1];
                  ip += 3;
                } else {
                  end = ip + 3 + bc[ip + 1] + bc[ip + 2];
                  ip += 3 + bc[ip + 1];
                }

                break;

              case 14:
                ends.push(end);
                ips.push(ip + 3 + bc[ip + 1] + bc[ip + 2]);

                if (stack[stack.length - 1] === peg$FAILED) {
                  end = ip + 3 + bc[ip + 1];
                  ip += 3;
                } else {
                  end = ip + 3 + bc[ip + 1] + bc[ip + 2];
                  ip += 3 + bc[ip + 1];
                }

                break;

              case 15:
                ends.push(end);
                ips.push(ip + 3 + bc[ip + 1] + bc[ip + 2]);

                if (stack[stack.length - 1] !== peg$FAILED) {
                  end = ip + 3 + bc[ip + 1];
                  ip += 3;
                } else {
                  end = ip + 3 + bc[ip + 1] + bc[ip + 2];
                  ip += 3 + bc[ip + 1];
                }

                break;

              case 16:
                if (stack[stack.length - 1] !== peg$FAILED) {
                  ends.push(end);
                  ips.push(ip);

                  end = ip + 2 + bc[ip + 1];
                  ip += 2;
                } else {
                  ip += 2 + bc[ip + 1];
                }

                break;

              case 17:
                ends.push(end);
                ips.push(ip + 3 + bc[ip + 1] + bc[ip + 2]);

                if (input.length > peg$currPos) {
                  end = ip + 3 + bc[ip + 1];
                  ip += 3;
                } else {
                  end = ip + 3 + bc[ip + 1] + bc[ip + 2];
                  ip += 3 + bc[ip + 1];
                }

                break;

              case 18:
                ends.push(end);
                ips.push(ip + 4 + bc[ip + 2] + bc[ip + 3]);

                if (input.substr(peg$currPos, peg$consts[bc[ip + 1]].length) === peg$consts[bc[ip + 1]]) {
                  end = ip + 4 + bc[ip + 2];
                  ip += 4;
                } else {
                  end = ip + 4 + bc[ip + 2] + bc[ip + 3];
                  ip += 4 + bc[ip + 2];
                }

                break;

              case 19:
                ends.push(end);
                ips.push(ip + 4 + bc[ip + 2] + bc[ip + 3]);

                if (input.substr(peg$currPos, peg$consts[bc[ip + 1]].length).toLowerCase() === peg$consts[bc[ip + 1]]) {
                  end = ip + 4 + bc[ip + 2];
                  ip += 4;
                } else {
                  end = ip + 4 + bc[ip + 2] + bc[ip + 3];
                  ip += 4 + bc[ip + 2];
                }

                break;

              case 20:
                ends.push(end);
                ips.push(ip + 4 + bc[ip + 2] + bc[ip + 3]);

                if (peg$consts[bc[ip + 1]].test(input.charAt(peg$currPos))) {
                  end = ip + 4 + bc[ip + 2];
                  ip += 4;
                } else {
                  end = ip + 4 + bc[ip + 2] + bc[ip + 3];
                  ip += 4 + bc[ip + 2];
                }

                break;

              case 21:
                stack.push(input.substr(peg$currPos, bc[ip + 1]));
                peg$currPos += bc[ip + 1];
                ip += 2;
                break;

              case 22:
                stack.push(peg$consts[bc[ip + 1]]);
                peg$currPos += peg$consts[bc[ip + 1]].length;
                ip += 2;
                break;

              case 23:
                stack.push(peg$FAILED);
                if (peg$silentFails === 0) {
                  peg$fail(peg$consts[bc[ip + 1]]);
                }
                ip += 2;
                break;

              case 24:
                peg$savedPos = stack[stack.length - 1 - bc[ip + 1]];
                ip += 2;
                break;

              case 25:
                peg$savedPos = peg$currPos;
                ip++;
                break;

              case 26:
                params = bc.slice(ip + 4, ip + 4 + bc[ip + 3]);
                for (i = 0; i < bc[ip + 3]; i++) {
                  params[i] = stack[stack.length - 1 - params[i]];
                }

                stack.splice(
                  stack.length - bc[ip + 2],
                  bc[ip + 2],
                  peg$consts[bc[ip + 1]].apply(null, params)
                );

                ip += 4 + bc[ip + 3];
                break;

              case 27:
                stack.push(peg$parseRule(bc[ip + 1]));
                ip += 2;
                break;

              case 28:
                peg$silentFails++;
                ip++;
                break;

              case 29:
                peg$silentFails--;
                ip++;
                break;

              default:
                throw new Error("Invalid opcode: " + bc[ip] + ".");
            }
          }

          if (ends.length > 0) {
            end = ends.pop();
            ip = ips.pop();
          } else {
            break;
          }
        }

        return stack[0];
      }

      peg$result = peg$parseRule(peg$startRuleIndex);

      if (peg$result !== peg$FAILED && peg$currPos === input.length) {
        return peg$result;
      } else {
        if (peg$result !== peg$FAILED && peg$currPos < input.length) {
          peg$fail(peg$endExpectation());
        }

        throw peg$buildStructuredError(
          peg$maxFailExpected,
          peg$maxFailPos < input.length ? input.charAt(peg$maxFailPos) : null,
          peg$maxFailPos < input.length
            ? peg$computeLocation(peg$maxFailPos, peg$maxFailPos + 1)
            : peg$computeLocation(peg$maxFailPos, peg$maxFailPos)
        );
      }
    }

    return {
      SyntaxError: peg$SyntaxError,
      parse:       peg$parse
    };
  })().parse;

  var workspaceDiv = document.getElementById('workspace');
  var inputTxt = document.getElementById('inputTxt');
  var nextBtn = document.getElementById('nextBtn');
  var workspaceContainerDiv = document.getElementById('workspaceContainer');
  var askBtn = document.getElementById('askBtn');
  var replyBtn = document.getElementById('replyBtn');
  var waitBtn = document.getElementById('waitBtn');
  var dummy = document.createElement('textarea');
  function escapeHTML(html) {
      dummy.textContent = html;
      return dummy.innerHTML;
  }
  function renderMessage(mapping, expansion, msg, isSubmessage) {
      if (isSubmessage === void 0) { isSubmessage = false; }
      switch (msg.tag) {
          case 'Text':
              return escapeHTML(msg.contents);
          case 'Reference':
              var p = msg.contents;
              if (p in expansion) {
                  return renderMessage(mapping, expansion, expansion[p], true);
              }
              else {
                  return '<span class="pointer" data-original="' + p + '">$' + mapping[p] + '</span>';
              }
          case 'Structured': {
              var msgs = msg.contents.map(function (m) { return renderMessage(mapping, expansion, m, true); });
              return isSubmessage ? '[' + msgs.join('') + ']' : msgs.join('');
          }
          case 'LabeledStructured': {
              var label = msg.contents[0];
              var msgs = msg.contents[1].map(function (m) { return renderMessage(mapping, expansion, m, true); });
              return '[$' + mapping[label] + '|' + msgs.join('') + ']';
          }
          default:
              throw "Something's wrong";
      }
  }
  function renderWorkspace(mapping, workspace) {
      console.log(workspace);
      var expansion = workspace.expandedPointers;
      var questionText = 'Question: ' + renderMessage(mapping, expansion, workspace.question);
      var subquestionText = workspace.subQuestions.map(function (q, i) {
          var answer = q[2];
          return '<br/>' + (i + 1) + '. ' + renderMessage(mapping, expansion, q[1])
              + (answer !== null ? '<br/>Answer: ' + renderMessage(mapping, expansion, answer) : '');
      }).join('<br/>');
      workspaceDiv.innerHTML = questionText + '<br/>' + subquestionText + '<br/>';
  }
  function mappingFromMessage(mapping, expansion, msg) {
      switch (msg.tag) {
          case 'Text':
              return;
          case 'Reference':
              var p = msg.contents;
              if (!(p in mapping)) {
                  mapping[p] = mapping.nextPointer++;
              }
              if (p in expansion) {
                  mappingFromMessage(mapping, expansion, expansion[p]);
              }
              return;
          case 'Structured':
              msg.contents.forEach(function (m) { return mappingFromMessage(mapping, expansion, m); });
              return;
          case 'LabeledStructured':
              var label = msg.contents[0];
              if (!(label in mapping)) {
                  mapping[label] = mapping.nextPointer++;
              }
              msg.contents[1].forEach(function (m) { return mappingFromMessage(mapping, expansion, m); });
              return;
          default:
              throw "Something's wrong";
      }
  }
  function mappingFromWorkspace(mapping, workspace) {
      var expansion = workspace.expandedPointers;
      mappingFromMessage(mapping, expansion, workspace.question);
      workspace.subQuestions.forEach(function (q) {
          var answer = q[2];
          mappingFromMessage(mapping, expansion, q[1]);
          if (answer !== null)
              mappingFromMessage(mapping, expansion, answer);
      });
  }
  function renumberMessage(mapping, msg) {
      switch (msg.tag) {
          case 'Text':
              return msg;
          case 'Reference':
              return { tag: 'Reference', contents: mapping[msg.contents] };
          case 'Structured':
              return { tag: 'Structured', contents: msg.contents.map(function (m) { return renumberMessage(mapping, m); }) };
          default:
              throw "Something's wrong";
      }
  }
  var User = (function () {
      function User(userId) {
          this.userId = userId;
          this.pending_ = [];
          this.workspace_ = null;
          this.mapping_ = { nextPointer: 0 };
          this.inverseMapping_ = {};
      }
      Object.defineProperty(User.prototype, "workspace", {
          get: function () { return this.workspace_; },
          enumerable: true,
          configurable: true
      });
      Object.defineProperty(User.prototype, "mapping", {
          get: function () { return this.mapping_; },
          enumerable: true,
          configurable: true
      });
      Object.defineProperty(User.prototype, "workspaceId", {
          get: function () { var ws = this.workspace; return ws === null ? null : ws.identity; },
          enumerable: true,
          configurable: true
      });
      User.prototype.updateInverseMapping = function () {
          for (var k in this.mapping) {
              if (k === 'nextPointer')
                  continue;
              this.inverseMapping_[this.mapping[k]] = parseInt(k, 10);
          }
      };
      User.prototype.postProcess = function (r) {
          switch (r.tag) {
              case 'OK':
                  this.pending_ = [];
                  this.workspace_ = null;
                  this.mapping_ = { nextPointer: 0 };
                  this.inverseMapping_ = {};
                  return r;
              case 'Error':
              default:
                  return r;
          }
      };
      User.prototype.ask = function (msg) {
          var msg2 = renumberMessage(this.inverseMapping_, msg);
          this.pending_.push({ Left: msg2 });
          this.workspace.subQuestions.push([null, msg2, null]);
      };
      User.prototype.reply = function (msg) {
          var _this = this;
          return postReply([{ userId: this.userId }, this.workspaceId, this.pending_, renumberMessage(this.inverseMapping_, msg)])
              .then(function (r) { return _this.postProcess(r.data); });
      };
      User.prototype.view = function (ptr) {
          var _this = this;
          return getPointer(ptr).then(function (r) {
              var msg = r.data;
              if (msg !== null) {
                  _this.pending_.push({ Right: ptr });
                  var expansion = _this.workspace.expandedPointers;
                  expansion[ptr] = msg;
                  mappingFromMessage(_this.mapping, expansion, msg);
                  _this.updateInverseMapping();
                  return { tag: 'OK' };
              }
              return { tag: 'Error' };
          });
      };
      User.prototype.wait = function () {
          var _this = this;
          return postWait([{ userId: this.userId }, this.workspaceId, this.pending_]).then(function (r) { return _this.postProcess(r.data); });
      };
      User.prototype.next = function () {
          var _this = this;
          return postNext({ userId: this.userId }).then(function (response) {
              var ws = response.data;
              if (ws === null)
                  return null;
              _this.workspace_ = ws;
              mappingFromWorkspace(_this.mapping, ws);
              _this.updateInverseMapping();
              return ws;
          });
      };
      return User;
  }());
  getJoin().then(function (joinResponse) {
      var user = new User(joinResponse.data.userId);
      workspaceDiv.addEventListener('click', function (evt) {
          var target = evt.target;
          if (target !== null && target.classList.contains('pointer')) {
              user.view(parseInt(target.dataset.original, 10)).then(function (r) {
                  if (r.tag === 'OK') {
                      renderWorkspace(user.mapping, user.workspace);
                  }
                  else {
                      console.log(r);
                  }
              });
              evt.preventDefault();
          }
      }, true);
      nextBtn.addEventListener('click', function (evt) {
          return user.next().then(function (ws) {
              if (ws === null) ;
              else {
                  renderWorkspace(user.mapping, ws);
                  workspaceContainerDiv.style.display = 'block';
                  nextBtn.style.display = 'none';
              }
          });
      });
      askBtn.addEventListener('click', function (evt) {
          var msg = messageParser(inputTxt.value);
          user.ask(msg);
          renderWorkspace(user.mapping, user.workspace);
          inputTxt.value = '';
      });
      replyBtn.addEventListener('click', function (evt) {
          var msg = messageParser(inputTxt.value);
          user.reply(msg).then(function (r) {
              if (r.tag === 'OK') {
                  workspaceContainerDiv.style.display = 'none';
                  nextBtn.style.display = 'inline';
                  inputTxt.value = '';
              }
              else {
                  console.log(r);
              }
          });
      });
      waitBtn.addEventListener('click', function (evt) {
          user.wait().then(function (r) {
              if (r.tag === 'OK') {
                  workspaceContainerDiv.style.display = 'none';
                  nextBtn.style.display = 'inline';
                  inputTxt.value = '';
              }
              else {
                  console.log(r);
              }
          });
      });
  }).catch(function (e) { return console.log(e); });

}());
