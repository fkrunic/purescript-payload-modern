"use strict";

export function encodeUri (str) {
  try {
    return encodeURIComponent(str)
  } catch (e) {
    console.error(e)
    return str
  }  
}

// exports.encodeUri = function(str) {
//   try {
//     return encodeURIComponent(str)
//   } catch (e) {
//     console.error(e)
//     return str
//   }
// }
