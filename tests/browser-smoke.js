const { JSDOM } = require("jsdom");
const fs = require("fs");
const path = require("path");

const dom = new JSDOM(`
  <body>
    <button id="appearance-button" aria-expanded="false"></button>
    <div id="appearance-panel" aria-hidden="true"><button data-action="close-appearance">Close</button></div>
    <button id="theme-toggle"></button>
    <button class="palette-option" data-palette="standard" aria-pressed="true"></button>
    <button class="palette-option" data-palette="protanopia" aria-pressed="false"></button>
    <details class="workflow-card" id="workflow-step-1" open><summary>Data</summary></details>
    <details class="workflow-card" id="workflow-step-2"><summary>Method</summary></details>
    <details class="workflow-card" id="workflow-step-3"><summary>Columns</summary></details>
    <button data-open-step="3">Continue</button>
    <button class="method-kind-button active" data-method-kind="statistics" aria-selected="true">Statistics</button>
    <button class="method-kind-button" data-method-kind="machine_learning" aria-selected="false">Machine learning</button>
    <div class="method-kind-panel active" data-method-kind-panel="statistics"></div>
    <div class="method-kind-panel" data-method-kind-panel="machine_learning"></div>
    <button class="result-tab active" data-tab="details"></button>
    <button class="result-tab" data-tab="checks"></button>
    <div id="result-panel-details" class="result-panel active"></div>
    <div id="result-panel-checks" class="result-panel"></div>
    <main id="main-content"></main>
    <a id="methods-trigger" href="#methods-panel">Methods</a>
    <div id="methods-panel" class="static-dialog-overlay" aria-hidden="true">
      <button class="static-dialog-backdrop" data-close-static-dialog="true">Close backdrop</button>
      <div class="static-dialog-card"><button class="static-dialog-close" data-close-static-dialog="true">×</button></div>
    </div>
    <select id="method"><option value="descriptives">Descriptions</option></select>
    <a data-modal-method="descriptives"><small class="method-data-fit"></small></a>
  </body>
`, { runScripts: "outside-only", url: "http://127.0.0.1" });

const { window } = dom;
const { document } = window;
window.__handlers = {};
window.Shiny = {
  addCustomMessageHandler(name, handler) { window.__handlers[name] = handler; },
  setInputValue() {},
};
window.eval(fs.readFileSync(path.resolve(__dirname, "../www/app.js"), "utf8"));
document.dispatchEvent(new window.Event("DOMContentLoaded"));

function check(value, message) {
  if (!value) throw new Error(message);
}

document.querySelector("#theme-toggle").click();
check(document.body.classList.contains("light-mode"), "Theme switch failed");

document.querySelector("#appearance-button").click();
check(document.querySelector("#appearance-panel").getAttribute("aria-hidden") === "false", "Color-vision panel failed");
document.querySelector('[data-palette="protanopia"]').click();
check(document.body.classList.contains("palette-protanopia"), "Palette switch failed");
document.querySelector('[data-action="close-appearance"]').click();
check(document.querySelector("#appearance-panel").getAttribute("aria-hidden") === "true", "Color-vision X did not close the panel");

document.querySelector('[data-method-kind="machine_learning"]').click();
check(document.querySelector('[data-method-kind-panel="machine_learning"]').classList.contains("active"), "Method-type switch failed");

document.querySelector('[data-open-step="3"]').click();
check(document.querySelector("#workflow-step-3").open, "Workflow card failed");

document.querySelector('[data-tab="checks"]').click();
check(document.querySelector("#result-panel-checks").classList.contains("active"), "Result tab failed");

window.__handlers["landing-visibility"]({ show: false });
check(document.body.classList.contains("landing-hidden"), "Landing switch failed");

window.__handlers["method-feasibility"]({
  items: [{ id: "descriptives", possible: false, reason: "Needs numeric columns." }],
});
check(document.querySelector('[data-modal-method="descriptives"]').classList.contains("is-unlikely"), "Method cue failed");
check(document.querySelector('#method option').textContent.startsWith("△"), "Method option cue failed");

window.history.replaceState(null, "", "/#methods-panel");
window.Daybreak.syncStaticDialog();
check(document.querySelector("#methods-panel").classList.contains("is-open"), "Dialog did not open");
check(document.querySelector("#methods-panel").getAttribute("aria-hidden") === "false", "Open dialog remained hidden to assistive technology");
document.querySelector(".static-dialog-close").click();
check(!document.querySelector("#methods-panel").classList.contains("is-open"), "Dialog X did not close the panel");
check(document.querySelector("#methods-panel").classList.contains("is-closed"), "Dialog X did not set the closed state");
check(window.location.hash === "", "Dialog X did not clear the URL target");

window.history.replaceState(null, "", "/#methods-panel");
window.Daybreak.syncStaticDialog();
document.querySelector(".static-dialog-backdrop").click();
check(document.querySelector("#methods-panel").getAttribute("aria-hidden") === "true", "Dialog backdrop did not close the panel");

console.log("Browser interactions passed");
