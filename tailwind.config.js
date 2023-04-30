/** @type {import('tailwindcss').Config} */
module.exports = {
  content: process.env.NODE_ENV == 'production' ? ["./resources/public/js/compiled/app.js"] : ["./src/**/*.cljs", "./resources/public/js/compiled/cljs-runtime/*.js"],
  theme: {
    extend: {},
  },
  plugins: [],
}