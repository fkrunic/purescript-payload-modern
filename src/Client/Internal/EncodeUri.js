"use strict";

export function encodeUri(str) {
  try {
    return encodeURIComponent(str);
  } catch (e) {
    return str;
  }
}
