import ReactUtils from 'react-dom/test-utils.js'

export function isNull(v) { return v === null; }
export function act(cb) {
  return function () {
    ReactUtils.act(() => {
      cb();
    });
  }
}
