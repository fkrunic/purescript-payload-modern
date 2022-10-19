"use strict";

export function onError (server, cb) {
  server.on("error", function(error) {
    cb(error)();
  })
}
