import {defineConfig} from 'vite'
import elmPlugin from 'vite-plugin-elm'
// vite.config.js
export default defineConfig({
  // config options
  root: "./public/",
  base: "/cirdis/",
  plugins: [elmPlugin()],
  build: {
    outDir: "../docs/",
    emptyOutDir: true
  }
})
