import ReactUtils from 'react-dom/test-utils'

export function isNull(v) { return v === null; }
export function act(cb) {
  return function () {
    window.__DEV__ = true
    ReactUtils.act(() => {
      cb();
    });
  }
}
