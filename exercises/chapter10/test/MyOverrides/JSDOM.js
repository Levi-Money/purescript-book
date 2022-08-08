import { JSDOM } from 'jsdom';

export function createJSDOMImpl(options) {
  return () => new JSDOM(``, options);
}

export function setGlobalWindow(dom) {
  return () => {
    global.window = dom.window;
  }
}
