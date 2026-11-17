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
    <div class="method-picker-shell">
      <input id="method-search" type="search">
      <p id="method-search-status"></p>
      <section class="method-family-section">
        <a class="method-name-choice" data-modal-method="descriptives" data-search="numeric summaries describe"><small class="method-data-fit"></small></a>
      </section>
      <section class="method-family-section">
        <a class="method-name-choice" data-modal-method="independent_t" data-search="independent samples t test compare groups"><small class="method-data-fit"></small></a>
      </section>
      <a class="method-guide-pick" data-modal-method="descriptives">Numeric summaries</a>
    </div>
    <div id="tour-panel" class="static-dialog-overlay" aria-hidden="true">
      <div class="static-dialog-card"><button class="static-dialog-close" data-close-static-dialog="true">×</button>
        <div class="tour" data-tour-count="3">
          <div class="tour-slide is-active" data-tour-slide="1"></div>
          <div class="tour-slide" data-tour-slide="2"></div>
          <div class="tour-slide" data-tour-slide="3"></div>
          <span class="tour-dot is-active" data-tour-dot="1"></span><span class="tour-dot" data-tour-dot="2"></span><span class="tour-dot" data-tour-dot="3"></span>
          <button class="tour-back" data-tour-step="-1" disabled>Back</button>
          <button class="tour-next" data-tour-step="1">Next</button>
          <button class="tour-done" data-tour-done="true" hidden>Get started</button>
        </div>
      </div>
    </div>
    <button class="model-card" data-model="gemma3:4b">Gemma</button>
    <button id="run_analysis">Run</button>
  </body>
`, { runScripts: "outside-only", url: "http://127.0.0.1" });

const { window } = dom;
const { document } = window;
window.__handlers = {};
window.__inputs = {};
// Shiny rejects a handler that does not declare exactly one argument, and the
// rejection is an exception. The stand-in applies the same rule, so a handler
// that would break the real page breaks this test.
window.Shiny = {
  addCustomMessageHandler(name, handler) {
    if (typeof handler !== "function" || handler.length !== 1) {
      throw new Error("handler must be a function that takes one argument: " + name);
    }
    window.__handlers[name] = handler;
  },
  setInputValue(name, value) { window.__inputs[name] = value; },
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

for (const name of ["scroll-main", "landing-visibility", "workflow-progress", "method-feasibility",
                    "restore-display", "close-static-dialog", "click-run"]) {
  check(typeof window.__handlers[name] === "function", "No handler was registered for " + name);
}

let runs = 0;
document.querySelector("#run_analysis").addEventListener("click", function () { runs += 1; });
window.__handlers["click-run"]({});
check(runs === 1, "The run message did not press the run button");

window.__handlers["workflow-progress"]({ step: 2 });
check(document.querySelector("#workflow-step-2").open, "Workflow message did not open its step");

check(document.querySelector(".method-guide-pick").classList.contains("is-unlikely"), "The guide pick did not dim with its card");

const search = document.querySelector("#method-search");
search.value = "groups";
search.dispatchEvent(new window.Event("input", { bubbles: true }));
check(document.querySelector('.method-name-choice[data-modal-method="descriptives"]').hidden, "Search left a card that does not match");
check(!document.querySelector('.method-name-choice[data-modal-method="independent_t"]').hidden, "Search hid a card that matches");
check(document.querySelectorAll(".method-family-section")[0].hidden, "Search left an empty family heading");
check(document.querySelector("#method-search-status").textContent === "1 method matches.", "Search did not report its count");
search.value = "zzzz";
search.dispatchEvent(new window.Event("input", { bubbles: true }));
check(document.querySelector("#method-search-status").textContent.startsWith("No method matches"), "Search did not report an empty result");
search.value = "";
search.dispatchEvent(new window.Event("input", { bubbles: true }));
check(!document.querySelector('.method-name-choice[data-modal-method="descriptives"]').hidden, "Clearing the search did not restore the list");

check(window.localStorage.getItem("daybreak-tour-seen") === "yes", "The walkthrough was not marked as seen on a first visit");
document.querySelector(".tour-next").click();
check(document.querySelector('[data-tour-slide="2"]').classList.contains("is-active"), "The walkthrough did not advance");
check(!document.querySelector(".tour-back").disabled, "Back stayed disabled past the first slide");
document.querySelector(".tour-next").click();
check(document.querySelector(".tour-next").hidden && !document.querySelector(".tour-done").hidden, "The last slide did not offer its closing button");
document.querySelector(".tour-next").click();
check(document.querySelector('[data-tour-slide="3"]').classList.contains("is-active"), "The walkthrough ran past its last slide");

document.querySelector(".model-card").click();
check(window.__inputs.model_choice === "gemma3:4b", "A model card did not report its model");

console.log("Browser interactions passed");
