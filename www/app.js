(function () {
  "use strict";

  const paletteClasses = [
    "palette-standard",
    "palette-protanopia",
    "palette-deuteranopia",
    "palette-tritanopia",
    "palette-monochrome",
  ];
  let dialogReturnFocus = null;

  function readSetting(name, fallback) {
    try {
      return window.localStorage.getItem(name) || fallback;
    } catch (error) {
      return fallback;
    }
  }

  function writeSetting(name, value) {
    try {
      window.localStorage.setItem(name, value);
    } catch (error) {
      return false;
    }
    return true;
  }

  function send(name, value) {
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue(name, value, { priority: "event" });
    }
  }

  function setPressed(selector, active) {
    document.querySelectorAll(selector).forEach(function (button) {
      button.setAttribute("aria-pressed", button === active ? "true" : "false");
    });
  }

  function setTheme(mode) {
    const selectedMode = mode === "light" ? "light" : "dark";
    document.body.classList.toggle("light-mode", selectedMode === "light");
    document.documentElement.style.colorScheme = selectedMode;

    const toggle = document.getElementById("theme-toggle");
    if (toggle) {
      const label = selectedMode === "light" ? "Switch to dark mode" : "Switch to light mode";
      toggle.setAttribute("aria-label", label);
      toggle.setAttribute("title", label);
    }
    const selectedButton = document.querySelector('[data-theme="' + selectedMode + '"]');
    setPressed(".theme-choice", selectedButton);
    writeSetting("daybreak-theme", selectedMode);
    send("theme_mode", selectedMode);
  }

  function setPalette(name, button) {
    const selected = paletteClasses.indexOf("palette-" + name) >= 0 ? name : "standard";
    paletteClasses.forEach(function (className) {
      document.body.classList.remove(className);
    });
    document.body.classList.add("palette-" + selected);

    const selectedButton = button || document.querySelector('[data-palette="' + selected + '"]');
    setPressed(".palette-option", selectedButton);
    writeSetting("daybreak-palette", selected);
    send("color_palette", selected);
  }

  function toggleAppearance() {
    const panel = document.getElementById("appearance-panel");
    const button = document.getElementById("appearance-button");
    if (!panel || !button) return;

    const opening = panel.getAttribute("aria-hidden") !== "false";
    panel.setAttribute("aria-hidden", opening ? "false" : "true");
    button.setAttribute("aria-expanded", opening ? "true" : "false");
    if (opening) {
      const first = panel.querySelector("button");
      if (first) first.focus();
    }
  }

  function closeAppearance(returnFocus) {
    const panel = document.getElementById("appearance-panel");
    const button = document.getElementById("appearance-button");
    const wasOpen = panel && panel.getAttribute("aria-hidden") === "false";
    if (panel) panel.setAttribute("aria-hidden", "true");
    if (button) {
      button.setAttribute("aria-expanded", "false");
      if (returnFocus && wasOpen) button.focus();
    }
  }

  const FOCUSABLE = [
    "a[href]",
    "button:not([disabled])",
    "input:not([disabled]):not([type=hidden])",
    "select:not([disabled])",
    "textarea:not([disabled])",
    "details > summary",
    "[tabindex]:not([tabindex='-1'])",
  ].join(", ");

  // aria-modal tells a screen reader to ignore the page behind the dialog, but
  // it does nothing for the Tab key, so a keyboard user could walk straight out
  // of the overlay into the sidebar underneath it. This keeps the cycle closed.
  function trapDialogFocus(event) {
    if (event.key !== "Tab") return;
    const panel = document.querySelector(".static-dialog-overlay.is-open");
    if (!panel) return;
    const items = Array.from(panel.querySelectorAll(FOCUSABLE)).filter(function (node) {
      return node.offsetParent !== null || node === document.activeElement;
    });
    if (!items.length) return;
    const first = items[0];
    const last = items[items.length - 1];
    if (event.shiftKey && document.activeElement === first) {
      last.focus();
      event.preventDefault();
    } else if (!event.shiftKey && document.activeElement === last) {
      first.focus();
      event.preventDefault();
    } else if (!panel.contains(document.activeElement)) {
      first.focus();
      event.preventDefault();
    }
  }

  function currentStaticDialog() {
    const id = window.location.hash.replace(/^#/, "");
    if (!/^[A-Za-z][A-Za-z0-9_-]*$/.test(id)) return null;
    const panel = document.getElementById(id);
    return panel && panel.classList.contains("static-dialog-overlay") ? panel : null;
  }

  function closeStaticDialog() {
    const panel = currentStaticDialog() || document.querySelector(".static-dialog-overlay.is-open");
    if (panel) {
      panel.classList.remove("is-open");
      panel.classList.add("is-closed");
      panel.setAttribute("aria-hidden", "true");
    }
    if (currentStaticDialog()) {
      window.history.replaceState(null, "", window.location.pathname + window.location.search);
    }
    document.body.classList.remove("static-dialog-open");
    if (dialogReturnFocus && document.contains(dialogReturnFocus)) dialogReturnFocus.focus();
    dialogReturnFocus = null;
  }

  function syncStaticDialog() {
    const panel = currentStaticDialog();
    const wasOpen = document.body.classList.contains("static-dialog-open");
    document.querySelectorAll(".static-dialog-overlay").forEach(function (item) {
      const open = item === panel;
      item.classList.toggle("is-open", open);
      if (open) item.classList.remove("is-closed");
      item.setAttribute("aria-hidden", open ? "false" : "true");
    });
    document.body.classList.toggle("static-dialog-open", Boolean(panel));
    if (panel) {
      const close = panel.querySelector(".static-dialog-close");
      if (close) window.setTimeout(function () { close.focus(); }, 0);
    } else if (wasOpen && dialogReturnFocus && document.contains(dialogReturnFocus)) {
      dialogReturnFocus.focus();
      dialogReturnFocus = null;
    }
  }

  function chooseMethod(id) {
    if (!id) return;
    const input = document.getElementById("method");
    if (input && input.querySelector('option[value="' + id + '"]')) {
      input.value = id;
      input.dispatchEvent(new Event("change", { bubbles: true }));
    }
    send("method_jump", id);
  }

  function chooseModalMethod(id) {
    chooseMethod(id);
    openWorkflowStep(3);
  }

  function openWorkflowStep(number) {
    document.querySelectorAll(".workflow-card").forEach(function (card) {
      const selected = card.id === "workflow-step-" + number;
      card.open = selected;
    });
    const selected = document.getElementById("workflow-step-" + number);
    if (selected) selected.classList.add("workflow-current");
  }

  function showMethodKind(kind, button) {
    document.querySelectorAll(".method-kind-button").forEach(function (item) {
      const active = item === button;
      item.classList.toggle("active", active);
      item.setAttribute("aria-selected", active ? "true" : "false");
    });
    document.querySelectorAll("[data-method-kind-panel]").forEach(function (panel) {
      const active = panel.getAttribute("data-method-kind-panel") === kind;
      panel.classList.toggle("active", active);
      panel.setAttribute("aria-hidden", active ? "false" : "true");
    });
  }

  function showResultTab(name, button) {
    document.querySelectorAll(".result-tab").forEach(function (tab) {
      tab.classList.remove("active");
      tab.setAttribute("aria-selected", "false");
      tab.setAttribute("tabindex", "-1");
    });
    document.querySelectorAll(".result-panel").forEach(function (panel) {
      panel.classList.remove("active");
      panel.setAttribute("aria-hidden", "true");
    });
    if (button) {
      button.classList.add("active");
      button.setAttribute("aria-selected", "true");
      button.setAttribute("tabindex", "0");
    }
    const panel = document.getElementById("result-panel-" + name);
    if (panel) {
      panel.classList.add("active");
      panel.setAttribute("aria-hidden", "false");
    }
  }

  function applyMethodFeasibility(message) {
    const items = message && Array.isArray(message.items) ? message.items : [];
    items.forEach(function (item) {
      if (!item || !item.id) return;
      const possible = item.possible === true;
      const option = document.querySelector('#method option[value="' + item.id + '"]');
      if (option) {
        // The marker used to be a bare triangle plus a title attribute, and a
        // screen reader announced neither usefully. The words go into the
        // option label so the reason is spoken with the method name.
        if (!option.dataset.originalLabel) {
          option.dataset.originalLabel = option.textContent
            .replace(/^△\s*/, "")
            .replace(/\s*\(check column types\)$/, "");
        }
        option.textContent = possible
          ? option.dataset.originalLabel
          : "△ " + option.dataset.originalLabel + " (check column types)";
        option.classList.toggle("method-option-unlikely", !possible);
        option.title = possible ? "" : item.reason || "The active columns may not fit this method.";
      }

      const card = document.querySelector('[data-modal-method="' + item.id + '"]');
      if (card) {
        card.classList.toggle("is-unlikely", !possible);
        if (possible) card.removeAttribute("title");
        else card.setAttribute("title", item.reason || "The active columns may not fit this method.");
        const note = card.querySelector(".method-data-fit");
        if (note) note.textContent = possible ? "" : "Check column types";
      }
    });
  }

  function toggleTheme() {
    setTheme(document.body.classList.contains("light-mode") ? "dark" : "light");
  }

  // Bound to both DOMContentLoaded and shiny:connected, and Shiny fires the
  // second one again after every reconnect. Without this guard the theme and
  // palette inputs were resent on each pass, which re-triggered downstream
  // reactives for no reason.
  let started = false;

  function initialize() {
    if (started) return;
    started = true;
    const theme = readSetting("daybreak-theme", "dark");
    const palette = readSetting("daybreak-palette", "standard");
    setTheme(theme);
    setPalette(palette);
    syncStaticDialog();

    if (window.Shiny && Shiny.addCustomMessageHandler && !window.__daybreakScrollHandler) {
      Shiny.addCustomMessageHandler("scroll-main", function () {
        const main = document.getElementById("main-content");
        if (main) {
          main.scrollIntoView({
            behavior: window.matchMedia("(prefers-reduced-motion: reduce)").matches ? "auto" : "smooth",
            block: "start",
          });
        }
      });
      Shiny.addCustomMessageHandler("landing-visibility", function (message) {
        document.body.classList.toggle("landing-hidden", message && message.show === false);
      });
      Shiny.addCustomMessageHandler("workflow-progress", function (message) {
        if (message && message.step) openWorkflowStep(message.step);
      });
      Shiny.addCustomMessageHandler("method-feasibility", applyMethodFeasibility);
      Shiny.addCustomMessageHandler("restore-display", function (message) {
        setTheme(message && message.theme ? message.theme : "dark");
        setPalette(message && message.palette ? message.palette : "standard");
      });
      Shiny.addCustomMessageHandler("close-static-dialog", function () {
        closeStaticDialog();
      });
      Shiny.addCustomMessageHandler("click-run", function () {
        const run = document.getElementById("run_analysis");
        if (run) run.click();
      });
      window.__daybreakScrollHandler = true;
    }

    if (window.jQuery && !window.__daybreakConnectionHandlers) {
      window.jQuery(document).on("shiny:connected.daybreak", function () {
        document.body.classList.remove("session-disconnected");
      });
      window.jQuery(document).on("shiny:disconnected.daybreak", function () {
        document.body.classList.add("session-disconnected");
      });
      window.__daybreakConnectionHandlers = true;
    }
  }

  document.addEventListener("click", function (event) {
    const target = event.target;
    if (!(target instanceof Element)) return;
    const appearanceButton = target.closest("#appearance-button");
    const themeButton = target.closest("#theme-toggle");
    const closeButton = target.closest('[data-action="close-appearance"]');
    const themeChoice = target.closest(".theme-choice[data-theme]");
    const paletteChoice = target.closest(".palette-option[data-palette]");
    const methodChoice = target.closest("[data-method]");
    const modalMethodChoice = target.closest("[data-modal-method]");
    const resultTab = target.closest(".result-tab[data-tab]");
    const workflowNext = target.closest("[data-open-step]");
    const methodKind = target.closest(".method-kind-button[data-method-kind]");
    const dialogTrigger = target.closest('a[href^="#"]');
    const dialogClose = target.closest(".static-dialog-close, .static-dialog-backdrop");

    if (dialogTrigger && !dialogClose) {
      const selector = dialogTrigger.getAttribute("href");
      if (selector && /^#[A-Za-z][A-Za-z0-9_-]*$/.test(selector)) {
        const panel = document.getElementById(selector.slice(1));
        if (panel && panel.classList.contains("static-dialog-overlay")) {
          panel.classList.remove("is-closed");
          dialogReturnFocus = dialogTrigger;
        }
      }
    }

    if (dialogClose) {
      event.preventDefault();
      closeStaticDialog();
    } else if (appearanceButton) toggleAppearance();
    else if (themeButton) toggleTheme();
    else if (closeButton) closeAppearance(true);
    else if (themeChoice) setTheme(themeChoice.getAttribute("data-theme"));
    else if (paletteChoice) setPalette(paletteChoice.getAttribute("data-palette"), paletteChoice);
    else if (modalMethodChoice) chooseModalMethod(modalMethodChoice.getAttribute("data-modal-method"));
    else if (methodChoice) chooseMethod(methodChoice.getAttribute("data-method"));
    else if (resultTab) showResultTab(resultTab.getAttribute("data-tab"), resultTab);
    else if (workflowNext) openWorkflowStep(workflowNext.getAttribute("data-open-step"));
    else if (methodKind) showMethodKind(methodKind.getAttribute("data-method-kind"), methodKind);

    const panel = document.getElementById("appearance-panel");
    const button = document.getElementById("appearance-button");
    if (
      panel &&
      button &&
      panel.getAttribute("aria-hidden") === "false" &&
      !panel.contains(target) &&
      !button.contains(target)
    ) {
      closeAppearance(false);
    }
  });

  // A quick-start card on the landing page picks the sample, then asks the
  // server to load it and run its starting method. Two inputs rather than one
  // because the sample selector in the sidebar has to move as well, so the
  // reader can see which example they are looking at.
  document.addEventListener("click", function (event) {
    const card = event.target.closest(".quick-start-card");
    if (!card) return;
    const sample = card.getAttribute("data-sample");
    if (!sample || typeof Shiny === "undefined") return;
    send("quick_start_sample", sample);
  });

  document.addEventListener("keydown", function (event) {
    trapDialogFocus(event);

    // Command or Control with Return runs the analysis from anywhere on the
    // page, so a reader who has just changed a setting does not have to hunt
    // for the button at the bottom of the sidebar.
    if ((event.metaKey || event.ctrlKey) && event.key === "Enter") {
      const run = document.getElementById("run_analysis");
      if (run && !run.disabled) {
        run.click();
        event.preventDefault();
        return;
      }
    }
    if (event.key === "Escape") {
      closeAppearance(true);
      closeStaticDialog();
    }
    if (
      event.target.classList &&
      event.target.classList.contains("result-tab") &&
      (event.key === "ArrowRight" || event.key === "ArrowLeft")
    ) {
      const tabs = Array.from(document.querySelectorAll(".result-tab"));
      const at = tabs.indexOf(event.target);
      const next = event.key === "ArrowRight"
        ? (at + 1) % tabs.length
        : (at - 1 + tabs.length) % tabs.length;
      tabs[next].click();
      tabs[next].focus();
      event.preventDefault();
    }
  });

  document.addEventListener("shiny:connected", initialize);
  window.addEventListener("hashchange", syncStaticDialog);
  document.addEventListener("shiny:connected", function () {
    document.body.classList.remove("session-disconnected");
  });
  document.addEventListener("shiny:disconnected", function () {
    document.body.classList.add("session-disconnected");
  });
  if (document.readyState !== "loading") initialize();
  else document.addEventListener("DOMContentLoaded", initialize);

  window.Daybreak = {
    chooseMethod: chooseMethod,
    chooseModalMethod: chooseModalMethod,
    openWorkflowStep: openWorkflowStep,
    closeAppearance: closeAppearance,
    closeStaticDialog: closeStaticDialog,
    setPalette: setPalette,
    setTheme: setTheme,
    showResultTab: showResultTab,
    showMethodKind: showMethodKind,
    syncStaticDialog: syncStaticDialog,
    applyMethodFeasibility: applyMethodFeasibility,
    toggleAppearance: toggleAppearance,
    toggleTheme: toggleTheme,
  };
})();
