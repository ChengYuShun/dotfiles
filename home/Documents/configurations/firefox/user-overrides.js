// Keep history.
user_pref("privacy.clearOnShutdown.cache", false);
user_pref("privacy.clearOnShutdown.cookies", false);
user_pref("privacy.clearOnShutdown.downloads", false);
user_pref("privacy.clearOnShutdown.formdata", false);
user_pref("privacy.clearOnShutdown.history", false);
user_pref("privacy.clearOnShutdown.sessions", false);

// Enable popup windows.
user_pref("dom.disable_open_during_load", false);

// Keep IPv6.
user_pref("network.dns.disableIPv6", false);

// Enable search engine.
user_pref("keyword.enabled", true);

// Disable RFP.
user_pref("privacy.resistFingerprinting", false);

// Disable margins.
user_pref("privacy.resistFingerprinting.letterboxing", false);

// Enable cross-origin referer.
user_pref("network.http.referer.XOriginPolicy", 0);

// Enable userChrome.css and userContent.css
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);
