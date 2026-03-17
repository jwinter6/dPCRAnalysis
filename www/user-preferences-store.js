(function (window, document) {
  "use strict";

  var DEFAULT_PREFS = {
    theme: {
      paletteName: "ggplot_default",
      primaryColor: null,
      accentColor: null
    },
    appSettings: {
      palette_id: "ggplot_default",
      export: {
        width: 5,
        height: 5,
        dpi: 96,
        format: "png"
      }
    },
    customPalettes: {}
  };

  function isObject(value) {
    return value !== null && typeof value === "object" && !Array.isArray(value);
  }

  function deepMerge(base, override) {
    var output = Array.isArray(base) ? base.slice() : Object.assign({}, base || {});

    if (!isObject(override)) {
      return output;
    }

    Object.keys(override).forEach(function (key) {
      var value = override[key];

      if (isObject(value) && isObject(output[key])) {
        output[key] = deepMerge(output[key], value);
        return;
      }

      output[key] = value;
    });

    return output;
  }

  function sanitizeUserId(userId) {
    if (typeof userId !== "string") {
      userId = userId == null ? "" : String(userId);
    }

    userId = userId.trim();
    return userId || "anonymous";
  }

  function storageKey(userId) {
    return "app:prefs:" + sanitizeUserId(userId);
  }

  function hasLocalStorage() {
    try {
      if (typeof window === "undefined" || !window.localStorage) {
        return false;
      }

      var testKey = "__dpcr_user_prefs__";
      window.localStorage.setItem(testKey, "1");
      window.localStorage.removeItem(testKey);
      return true;
    } catch (error) {
      return false;
    }
  }

  function safeJsonParse(value) {
    if (!value) {
      return null;
    }

    try {
      return JSON.parse(value);
    } catch (error) {
      return null;
    }
  }

  function setCookie(name, value, days) {
    var expires = new Date(Date.now() + (days || 365) * 864e5).toUTCString();
    document.cookie = encodeURIComponent(name) + "=" + encodeURIComponent(value) +
      "; expires=" + expires + "; path=/; SameSite=Lax";
  }

  function getCookie(name) {
    var cookies = document.cookie ? document.cookie.split("; ") : [];

    for (var i = 0; i < cookies.length; i += 1) {
      var parts = cookies[i].split("=");
      var key = decodeURIComponent(parts.shift() || "");

      if (key === name) {
        return decodeURIComponent(parts.join("="));
      }
    }

    return null;
  }

  function deleteCookie(name) {
    document.cookie = encodeURIComponent(name) +
      "=; expires=Thu, 01 Jan 1970 00:00:00 GMT; path=/; SameSite=Lax";
  }

  function buildPreferences(defaults, prefs) {
    return deepMerge(deepMerge({}, DEFAULT_PREFS), deepMerge(defaults || {}, prefs || {}));
  }

  function getQueryParam(name) {
    try {
      var params = new URLSearchParams(window.location.search || "");
      var value = params.get(name);
      return value && value.trim() ? value.trim() : null;
    } catch (error) {
      return null;
    }
  }

  function resolveUserId(explicitUserId) {
    return sanitizeUserId(explicitUserId || getQueryParam("userId") || getQueryParam("user_id") || "anonymous");
  }

  function isValidHex(value) {
    return typeof value === "string" && /^#[0-9A-F]{6}$/i.test(value.trim());
  }

  function applyTheme(theme) {
    var resolvedTheme = isObject(theme) ? theme : {};
    var root = document.documentElement;
    var paletteName = resolvedTheme.paletteName || DEFAULT_PREFS.theme.paletteName;

    root.setAttribute("data-app-palette", paletteName);

    if (isValidHex(resolvedTheme.primaryColor)) {
      root.style.setProperty("--app-primary-color", resolvedTheme.primaryColor.trim());
    } else {
      root.style.removeProperty("--app-primary-color");
    }

    if (isValidHex(resolvedTheme.accentColor)) {
      root.style.setProperty("--app-accent-color", resolvedTheme.accentColor.trim());
    } else {
      root.style.removeProperty("--app-accent-color");
    }
  }

  function pushToShiny(inputId, payload) {
    if (!window.Shiny || typeof window.Shiny.setInputValue !== "function") {
      return;
    }

    window.Shiny.setInputValue(inputId, payload, { priority: "event" });
  }

  var userPreferencesStore = {
    load: function (userId, defaults) {
      var key = storageKey(userId);
      var parsed = null;

      if (hasLocalStorage()) {
        try {
          parsed = safeJsonParse(window.localStorage.getItem(key));
        } catch (error) {
          parsed = null;
        }
      }

      if (!parsed) {
        try {
          parsed = safeJsonParse(getCookie(key));
        } catch (error) {
          parsed = null;
        }
      }

      return buildPreferences(defaults, parsed);
    },

    save: function (userId, prefs, defaults) {
      var key = storageKey(userId);
      var value = JSON.stringify(buildPreferences(defaults, prefs));

      try {
        if (hasLocalStorage()) {
          window.localStorage.setItem(key, value);
          return;
        }
      } catch (error) {
        // Cookie fallback below.
      }

      try {
        setCookie(key, value, 365);
      } catch (error) {
        // Intentionally silent: persistence is optional.
      }
    },

    clear: function (userId) {
      var key = storageKey(userId);

      try {
        if (hasLocalStorage()) {
          window.localStorage.removeItem(key);
        }
      } catch (error) {
        // ignore
      }

      try {
        deleteCookie(key);
      } catch (error) {
        // ignore
      }
    }
  };

  function handleInit(payload) {
    var resolvedUserId = resolveUserId(payload && payload.userId);
    var defaults = payload && payload.defaults ? payload.defaults : DEFAULT_PREFS;
    var preferences = userPreferencesStore.load(resolvedUserId, defaults);

    applyTheme(preferences.theme);
    pushToShiny("client_user_preferences_loaded", {
      userId: resolvedUserId,
      preferences: preferences
    });
  }

  function handleSave(payload) {
    var resolvedUserId = resolveUserId(payload && payload.userId);
    var defaults = payload && payload.defaults ? payload.defaults : DEFAULT_PREFS;
    var preferences = buildPreferences(defaults, payload && payload.preferences ? payload.preferences : {});

    applyTheme(preferences.theme);
    userPreferencesStore.save(resolvedUserId, preferences, defaults);
  }

  function handleClear(payload) {
    var resolvedUserId = resolveUserId(payload && payload.userId);
    userPreferencesStore.clear(resolvedUserId);
  }

  function registerShinyHandlers() {
    if (!window.Shiny || typeof window.Shiny.addCustomMessageHandler !== "function") {
      return;
    }

    if (registerShinyHandlers._registered) {
      return;
    }

    registerShinyHandlers._registered = true;
    window.Shiny.addCustomMessageHandler("userPreferences:init", handleInit);
    window.Shiny.addCustomMessageHandler("userPreferences:save", handleSave);
    window.Shiny.addCustomMessageHandler("userPreferences:clear", handleClear);
  }

  window.userPreferencesStore = userPreferencesStore;
  window.dPCRUserPreferences = {
    resolveUserId: resolveUserId,
    applyTheme: applyTheme,
    init: handleInit,
    save: handleSave,
    clear: handleClear
  };

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", registerShinyHandlers, { once: true });
  } else {
    registerShinyHandlers();
  }

  document.addEventListener("shiny:connected", registerShinyHandlers);
})(window, document);
