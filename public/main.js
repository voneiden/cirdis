import "../styles/views.scss"
import {Elm as Cirdis} from "../src/Main.elm";

const cirdis = Cirdis.Main.init({})


// Mouse move perf hack
let lastMouseMove = 0;
let sendDrag = false;

document.addEventListener("mousemove", (e) => {
  let now = Date.now()
  // 30 fps? 25 fps looks bad
  // TODO improve blocking so that last event always gets sent
  // Or remove this completely as it's no longer that cpu heavy..
  if (now - lastMouseMove < 33) {
    return;
  }
  const canvas = document.getElementById('canvas')
  if (!canvas.contains(e.target)) {
    return;
  }
  cirdis.ports.mouseDrag.send(e)
  lastMouseMove = now
})
document.addEventListener("mouseup", () => {
  sendDrag = false;
})


// Scroll throttling
let lastWheel = 0;
let sendWheel = false;
let deltaWheel = 0;
document.addEventListener("wheel", (e) => {
  let now = Date.now();
  deltaWheel += e.deltaY
  if (now - lastWheel < 33 || !sendWheel) {
    return;
  }
  cirdis.ports.wheel.send(deltaWheel)

  deltaWheel = 0;
  e.preventDefault()
  e.stopPropagation()
  e.stopImmediatePropagation()
  return false;
})

cirdis.ports.startWheel.subscribe(() => {
  sendWheel = true;
})

cirdis.ports.endWheel.subscribe(() => {
  sendWheel = true;
})

const sendCanvasSize = function sendCanvasSize() {
  const canvasContainer = document.getElementById('canvas-container')
  console.log("got container", canvasContainer)
  if (canvasContainer) {
    cirdis.ports.resize.send(canvasContainer.getBoundingClientRect())
  }
}
window.addEventListener('resize', (e) => {
  // Sync flexbox size to elm
  sendCanvasSize()
});


cirdis.ports.canvasSize.subscribe(() => {
  requestAnimationFrame(() => {
    sendCanvasSize()
  })

})


cirdis.ports.checkImages.subscribe(() => {
  requestAnimationFrame(() => {

    const images = Array.from(document.querySelectorAll("svg image"))
    const imageInformations = images.filter(i => i.id.startsWith('layer-')).map(i => {
      const tempImage = new Image()
      tempImage.src = i.href.baseVal
      return {
        layer: i.className.baseVal.slice(6),
        width: tempImage.width,
        height: tempImage.height
      }
    })

    cirdis.ports.imageInformation.send(imageInformations)

  })
})

/* External layer handling */
const cirdisLayersNode = document.createElementNS("http://www.w3.org/2000/svg", 'g')
cirdis.ports.setLayers.subscribe((layers) => {
  console.log("GOT layers", layers)
  while (cirdisLayersNode.firstChild) {
    cirdisLayersNode.firstChild.remove()
  }
  for (const layer of layers) {
    const image = document.createElementNS("http://www.w3.org/2000/svg", "image")
    image.setAttribute("id", "layer-" + layer.id.toString())
    image.setAttributeNS("http://www.w3.org/1999/xlink", "xlink:href", "data:" + layer.mimeType + ";base64," + layer.b64Data)
    cirdisLayersNode.appendChild(image)
  }
})

/* We use this mean trick to force an elm app inside an elm app
* The whole reason for this is performance - re-rending the image is very costly
* so we want to do it only when absolutely necessary (not on every mousemove!) */
function forceMount() {
  const mountpoint = document.getElementById('cirdis-layers-mountpoint')
  if (mountpoint) {
    if (!mountpoint.contains(cirdisLayersNode)) {
      mountpoint.appendChild(cirdisLayersNode)
      console.log("Mounted cirdisLayersNode")
    }
  }
  window.requestAnimationFrame(forceMount)
}

window.requestAnimationFrame(forceMount)
