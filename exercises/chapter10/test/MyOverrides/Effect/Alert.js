export function confirm(message) {
  return () => window.confirm(message)
}
