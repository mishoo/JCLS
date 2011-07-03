/*
 * A JavaScript implementation of the RSA Data Security, Inc. MD5 Message
 * Digest Algorithm, as defined in RFC 1321.
 * Version 2.1 Copyright (C) Paul Johnston 1999 - 2002.
 * Other contributors: Greg Holt, Andrew Kepert, Ydnar, Lostinet
 * Distributed under the BSD License
 * See http://pajhome.org.uk/crypt/md5 for more info.
 *
 * Adapted for DynarchLIB by Mihai Bazon.
 */(function(){function q(a){var c="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/",d="";for(var e=0;e<a.length*4;e+=3){var f=(a[e>>2]>>8*(e%4)&255)<<16|(a[e+1>>2]>>8*((e+1)%4)&255)<<8|a[e+2>>2]>>8*((e+2)%4)&255;for(var g=0;g<4;g++)e*8+g*6>a.length*32?d+=b:d+=c.charAt(f>>6*(3-g)&63)}return d}function p(b){var c=a?"0123456789ABCDEF":"0123456789abcdef",d="";for(var e=0;e<b.length*4;e++)d+=c.charAt(b[e>>2]>>e%4*8+4&15)+c.charAt(b[e>>2]>>e%4*8&15);return d}function o(a){var b="",d=(1<<c)-1;for(var e=0;e<a.length*32;e+=c)b+=String.fromCharCode(a[e>>5]>>>e%32&d);return b}function n(a){var b=[],d=(1<<c)-1;for(var e=0;e<a.length*c;e+=c)b[e>>5]|=(a.charCodeAt(e/c)&d)<<e%32;return b}function m(a,b){return a<<b|a>>>32-b}function l(a,b){var c=(a&65535)+(b&65535),d=(a>>16)+(b>>16)+(c>>16);return d<<16|c&65535}function k(a,b){var d=n(a);d.length>16&&(d=e(d,a.length*c));var f=Array(16),g=Array(16);for(var h=0;h<16;h++)f[h]=d[h]^909522486,g[h]=d[h]^1549556828;var i=e(f.concat(n(b)),512+b.length*c);return e(g.concat(i),640)}function j(a,b,c,d,e,g,h){return f(c^(b|~d),a,b,e,g,h)}function i(a,b,c,d,e,g,h){return f(b^c^d,a,b,e,g,h)}function h(a,b,c,d,e,g,h){return f(b&d|c&~d,a,b,e,g,h)}function g(a,b,c,d,e,g,h){return f(b&c|~b&d,a,b,e,g,h)}function f(a,b,c,d,e,f){return l(m(l(l(b,a),l(d,f)),e),c)}function e(a,b){a[b>>5]|=128<<b%32,a[(b+64>>>9<<4)+14]=b;var c=1732584193,d=-271733879,e=-1732584194,f=271733878;for(var k=0;k<a.length;k+=16){var m=c,n=d,o=e,p=f;c=g(c,d,e,f,a[k+0],7,-680876936),f=g(f,c,d,e,a[k+1],12,-389564586),e=g(e,f,c,d,a[k+2],17,606105819),d=g(d,e,f,c,a[k+3],22,-1044525330),c=g(c,d,e,f,a[k+4],7,-176418897),f=g(f,c,d,e,a[k+5],12,1200080426),e=g(e,f,c,d,a[k+6],17,-1473231341),d=g(d,e,f,c,a[k+7],22,-45705983),c=g(c,d,e,f,a[k+8],7,1770035416),f=g(f,c,d,e,a[k+9],12,-1958414417),e=g(e,f,c,d,a[k+10],17,-42063),d=g(d,e,f,c,a[k+11],22,-1990404162),c=g(c,d,e,f,a[k+12],7,1804603682),f=g(f,c,d,e,a[k+13],12,-40341101),e=g(e,f,c,d,a[k+14],17,-1502002290),d=g(d,e,f,c,a[k+15],22,1236535329),c=h(c,d,e,f,a[k+1],5,-165796510),f=h(f,c,d,e,a[k+6],9,-1069501632),e=h(e,f,c,d,a[k+11],14,643717713),d=h(d,e,f,c,a[k+0],20,-373897302),c=h(c,d,e,f,a[k+5],5,-701558691),f=h(f,c,d,e,a[k+10],9,38016083),e=h(e,f,c,d,a[k+15],14,-660478335),d=h(d,e,f,c,a[k+4],20,-405537848),c=h(c,d,e,f,a[k+9],5,568446438),f=h(f,c,d,e,a[k+14],9,-1019803690),e=h(e,f,c,d,a[k+3],14,-187363961),d=h(d,e,f,c,a[k+8],20,1163531501),c=h(c,d,e,f,a[k+13],5,-1444681467),f=h(f,c,d,e,a[k+2],9,-51403784),e=h(e,f,c,d,a[k+7],14,1735328473),d=h(d,e,f,c,a[k+12],20,-1926607734),c=i(c,d,e,f,a[k+5],4,-378558),f=i(f,c,d,e,a[k+8],11,-2022574463),e=i(e,f,c,d,a[k+11],16,1839030562),d=i(d,e,f,c,a[k+14],23,-35309556),c=i(c,d,e,f,a[k+1],4,-1530992060),f=i(f,c,d,e,a[k+4],11,1272893353),e=i(e,f,c,d,a[k+7],16,-155497632),d=i(d,e,f,c,a[k+10],23,-1094730640),c=i(c,d,e,f,a[k+13],4,681279174),f=i(f,c,d,e,a[k+0],11,-358537222),e=i(e,f,c,d,a[k+3],16,-722521979),d=i(d,e,f,c,a[k+6],23,76029189),c=i(c,d,e,f,a[k+9],4,-640364487),f=i(f,c,d,e,a[k+12],11,-421815835),e=i(e,f,c,d,a[k+15],16,530742520),d=i(d,e,f,c,a[k+2],23,-995338651),c=j(c,d,e,f,a[k+0],6,-198630844),f=j(f,c,d,e,a[k+7],10,1126891415),e=j(e,f,c,d,a[k+14],15,-1416354905),d=j(d,e,f,c,a[k+5],21,-57434055),c=j(c,d,e,f,a[k+12],6,1700485571),f=j(f,c,d,e,a[k+3],10,-1894986606),e=j(e,f,c,d,a[k+10],15,-1051523),d=j(d,e,f,c,a[k+1],21,-2054922799),c=j(c,d,e,f,a[k+8],6,1873313359),f=j(f,c,d,e,a[k+15],10,-30611744),e=j(e,f,c,d,a[k+6],15,-1560198380),d=j(d,e,f,c,a[k+13],21,1309151649),c=j(c,d,e,f,a[k+4],6,-145523070),f=j(f,c,d,e,a[k+11],10,-1120210379),e=j(e,f,c,d,a[k+2],15,718787259),d=j(d,e,f,c,a[k+9],21,-343485551),c=l(c,m),d=l(d,n),e=l(e,o),f=l(f,p)}return[c,d,e,f]}function d(){return hex_md5("abc")=="900150983cd24fb0d6963f7d28e17f72"}var a=0,b="",c=8;window.hex_md5=function(a){return p(e(n(a),a.length*c))},window.b64_md5=function(a){return q(e(n(a),a.length*c))},window.str_md5=function(a){return o(e(n(a),a.length*c))},window.hex_hmac_md5=function(a,b){return p(k(a,b))},window.b64_hmac_md5=function(a,b){return q(k(a,b))},window.str_hmac_md5=function(a,b){return o(k(a,b))}})()