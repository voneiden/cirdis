import "../styles/views.scss"
import {Elm} from "../src/Main.elm";

const parseStorage = function parseStorage() {
  return JSON.parse(localStorage.getItem('jalava'))
}

const app = Elm.Main.init({})


// Mouse move perf hack
let lastMouseMove = 0;
let sendDrag = false;

document.addEventListener("mousemove", (e) => {
  let now = Date.now()
  // 30 fps? 25 fps looks bad
  if (now - lastMouseMove < 33) {
    console.log("Block event")
    return;
  }
  console.log("Pass event", e)
  app.ports.mouseDrag.send(e)
  lastMouseMove = now
})
document.addEventListener("mouseup", () => {
  sendDrag = false;
})
app.ports.startDrag.subscribe(() => {
  sendDrag = true;
})

// Scroll throttling
let lastWheel = 0;
let sendWheel = false;
let deltaWheel = 0;
document.addEventListener("wheel", (e) => {
  let now = Date.now();
  deltaWheel += e.deltaY
  if (now - lastWheel < 33 || !sendWheel) {
    console.log("Block scroll", e)
    return;
  }
  console.log("scrlll", e)
  app.ports.wheel.send(deltaWheel)

  deltaWheel = 0;
  e.preventDefault()
  e.stopPropagation()
  e.stopImmediatePropagation()
  return false;
})

app.ports.startWheel.subscribe(() => {
  sendWheel = true;
})

app.ports.endWheel.subscribe(() => {
  sendWheel = true;
})

const sendCanvasSize = function sendCanvasSize() {
  const canvasContainer = document.getElementById('canvas-container')
  console.log("got container", canvasContainer)
  if (canvasContainer) {
    app.ports.resize.send(canvasContainer.getBoundingClientRect())
  }
}
window.addEventListener('resize', (e) => {
  // Sync flexbox size to elm
  sendCanvasSize()
});


app.ports.canvasSize.subscribe(() => {
  requestAnimationFrame(() => {
    sendCanvasSize()
  })

})


app.ports.checkImages.subscribe(() => {
  requestAnimationFrame(() => {

    const images = Array.from(document.querySelectorAll("svg image"))
    const imageInformations = images.filter(i => i.className.baseVal.startsWith('layer-')).map(i => {
      const tempImage = new Image()
      tempImage.src = i.href.baseVal
      return {
        layer: i.className.baseVal.slice(6),
        width: tempImage.width,
        height: tempImage.height
      }
    })

    app.ports.imageInformation.send(imageInformations)

  })
})
