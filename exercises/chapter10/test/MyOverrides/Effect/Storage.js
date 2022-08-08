export function removeItem(key) {
  return () => {
    window.localStorage.removeItem(key);
  }
}
