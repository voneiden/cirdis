import "../styles/views.scss"
import {Elm as Cirdis} from "../src/Main.elm";

const cirdis = Cirdis.Main.init({})


// Mouse move perf hack
let lastMouseMove = 0;
let delayedDeliver = null;
let sendDrag = false;
const onMouseMove = function onMouseMove(e) {
  // Create a separate object from the event to avoid breaking firefox
  return onMouseDrag({
    button: e.button,
    timeStamp: e.timeStamp,
    offsetX: e.offsetX,
    offsetY: e.offsetY,
    target: e.target
  })
}

const onMouseDrag = function onMouseDrag(e) {
  if (delayedDeliver) {
    window.clearTimeout(delayedDeliver)
    delayedDeliver = null
  }
  let now = Date.now()
  // 30 fps? 25 fps looks bad
  // TODO improve blocking so that last event always gets sent
  // Or remove this completely as it's no longer that cpu heavy..
  const timeSinceLastDeliveredEvent = now - lastMouseMove
  if (timeSinceLastDeliveredEvent < 33) {
    delayedDeliver = window.setTimeout(() => onMouseDrag(e), 34 - timeSinceLastDeliveredEvent)
    return;
  }
  const canvas = document.getElementById('canvas')
  if (!canvas.contains(e.target)) {
    return;
  }
  cirdis.ports.mouseDrag.send(e)
  lastMouseMove = now
}

document.addEventListener("mousemove", onMouseMove)
document.addEventListener("mouseup", () => {
  sendDrag = false;
})

// Scroll throttling
let lastWheel = 0;
let sendWheel = false;
let deltaWheel = 0;
document.addEventListener("wheel", (e) => {
  let now = Date.now();

  // on MacOS when shift is pressed it reports deltaX instead of deltaY
  // so we can check if shift is pressed and deltaY is 0, then use deltaX
  if (e.shiftKey && e.deltaY === 0) {
    deltaWheel += e.deltaX
  } else {
    deltaWheel += e.deltaY
  }
  cirdis.ports.wheel.send(deltaWheel)

  deltaWheel = 0;
  e.preventDefault()
  e.stopPropagation()
  e.stopImmediatePropagation()
  return false;
}, {passive: false})

cirdis.ports.startWheel.subscribe(() => {
  sendWheel = true;
})

cirdis.ports.endWheel.subscribe(() => {
  sendWheel = true;
})

const sendCanvasSize = function sendCanvasSize() {
  const canvasContainer = document.getElementById('canvas-container')
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

// Keydown event handling
let keyDownPreventDefault = true
cirdis.ports.keyDownPreventDefault.subscribe(() => {
  keyDownPreventDefault = true
})
cirdis.ports.keyDownAllowDefault.subscribe(() => {
  keyDownPreventDefault = false
})

const ignoreKeyCodes = [116]
const ignoreCtrlKeyCodes = [82, 48]
document.addEventListener('keydown', (e) => {

  if (ignoreKeyCodes.includes(e.keyCode)) {
    return
  }
  if (e.ctrlKey && ignoreCtrlKeyCodes.includes(e.keyCode)) {
    return
  }
  cirdis.ports.keyDown.send({
    keyCode: e.keyCode,
    shift: e.shiftKey,
    ctrl: e.ctrlKey
  })

  if (keyDownPreventDefault) {
    e.preventDefault()
  }

})

const checkImages = function checkImages(retryAttempt) {
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
}


/* External layer handling */
const cirdisLayersNode = document.createElementNS("http://www.w3.org/2000/svg", 'g')
cirdis.ports.setLayers.subscribe(([layers, checkImage]) => {
  while (cirdisLayersNode.firstChild) {
    cirdisLayersNode.firstChild.remove()
  }
  for (const layer of layers.reverse()) {
    const image = document.createElementNS("http://www.w3.org/2000/svg", "image")
    image.setAttribute("id", "layer-" + layer.id.toString())
    image.setAttributeNS("http://www.w3.org/1999/xlink", "xlink:href", "data:" + layer.mimeType + ";base64," + layer.b64Data)
    cirdisLayersNode.appendChild(image)
  }

  if (checkImage) {
    // There appears to be no guarantee whether the svg is actually there yet or not if we call
    // without requestAnimationFrame. Sometimes it is, sometimes it isn't.
    window.requestAnimationFrame(() => checkImages())
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


/* Project save
 * Grab the canvas outerHTML and hand it as string back to Elm
 */
const saveProject = function saveProject(retryCount = 0) {
  const canvas = document.getElementById("canvas")
  if (!canvas.getElementById('cirdis-src')) {
    if (retryCount < 10) {
      return window.requestAnimationFrame(() => saveProject(retryCount + 1))
    } else {
      return cirdis.ports.downloadProjectFailure.send(null)
    }

  }
  cirdis.ports.downloadProject.send(canvas.outerHTML)
}
cirdis.ports.saveProject.subscribe(() => {
  saveProject()
})


/* Project load
 * Grab the string which is hopefully valid SVG and jam it into a node. Then grab relevant data from the node
 * and pass it back to Elm
 */
cirdis.ports.loadSvg.subscribe((svgData) => {
  try {
    const parent = document.createElement('div')
    parent.innerHTML = svgData
    const layerDataElements = parent.querySelectorAll('[id^=\'layer-\']')
    // Not terribly robust, eh
    const layerData = Array.from(layerDataElements).map((lde) => [parseInt(lde.id.slice(6)), lde.href.baseVal.split(',')[1]])
    const data = parent.querySelector('#cirdis-src')?.dataset?.cirdisSrc

    if (data) {
      cirdis.ports.loadProjectSuccess.send({data: data, layerData: layerData})
      return
    } else {
      console.error("Got no data!", parent, data, data?.dataset)
    }
  } catch (e) {
    // This should "never" happen :-D
    console.error("Uncaught exception", e)
  }
  cirdis.ports.loadProjectFailure.send(null)
})
