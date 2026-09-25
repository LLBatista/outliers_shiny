// Draw Fluent's icons with Font Awesome (free licence, served with the app)
// instead of Microsoft's icon font, which is loaded from Microsoft's servers.
//
// Fluent refers to icons by name (e.g. "ChevronDown" for the dropdown arrow).
// Here each name we use is re-registered as a Font Awesome character.
// To add an icon: find its code on fontawesome.com (e.g. f078 for chevron-down)
// and add a line below.
(function () {
  var fluent = window.jsmodule["@fluentui/react"];

  var icons = {
    // Used by Fluent itself
    ChevronDown: "", // dropdown arrow; calendar "next month"
    Down: "", // calendar "next month"
    Up: "", // calendar "previous month"
    Calendar: "", // date picker field
    // Used on our buttons (iconProps = list(iconName = ...))
    Forward: "", // Start
    CheckMark: "" // Save, Submit
  };

  fluent.unregisterIcons(Object.keys(icons));
  fluent.registerIcons({
    // The Font Awesome font itself is already loaded by shiny$icon().
    fontFace: { fontFamily: '"Font Awesome 6 Free"' },
    // The weight (900 = Font Awesome's "solid" style) is set in main.scss.
    icons: icons
  });
})();
