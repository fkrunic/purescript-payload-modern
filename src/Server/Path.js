"use strict";

export function resolve(paths) {
  return require("path").resolve.apply(this, paths);
}
