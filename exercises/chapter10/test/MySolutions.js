"use strict";

export function volumeFn(x, y, z) {
  return x * y * z
}

export const volumeArrow = x => y => z => volumeFn(x, y, z)
