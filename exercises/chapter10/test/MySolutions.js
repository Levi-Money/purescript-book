"use strict";

export function volumeFn(x, y, z) {
  return x * y * z
}

export const volumeArrow = x => y => z => volumeFn(x, y, z)

function complex(real, imag) {
  return { real, imag }
}

function sumComplex(c1, c2) {
  return complex(
    c1.real + c2.real,
    c1.imag + c2.imag,
  )
}

export function cumulativeSumsComplex(arr) {
  return arr.reduce(
    (acc, cur) => {
      const sum = sumComplex(acc.sum, cur)
      const sums = [...acc.sums, sum]
      return { sum, sums }
    },
    { sum: complex(0, 0), sums: [] }
  ).sums;
}
