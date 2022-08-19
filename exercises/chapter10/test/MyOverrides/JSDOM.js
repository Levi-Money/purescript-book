import { JSDOM } from 'jsdom';

export const createJSDOMImpl = tmpl => options => () => new JSDOM(tmpl, options)

export function setGlobalWindow(dom) {
  return () => {
    global.window = dom.window;
  }
}
