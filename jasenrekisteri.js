document.addEventListener("DOMContentLoaded", function () {
  console.info("Initialising jasenrekisteri");

  // needle
  var filterNeedleSource = menrva.source("");
  var filterNeedle = filterNeedleSource.map(function (v) {
      return v.trim()
          .toLowerCase()
          .replace(/[^a-zäö ]+/g, "")
          .split(/ +/)
          .filter(function (x) { return x.length >= 2; });
  }, _.isEqual);

  // events
  var filterInput = document.querySelector("input#member-filter");
  if (filterInput) {
    filterInput.addEventListener("keyup", function () {
      menrva.transaction()
        .set(filterNeedleSource, filterInput.value)
        .commit();
    });
  }

  var rows = document.querySelectorAll("table#member-list tbody tr");

  // update
  filterNeedle.onValue(function (value) {
    if (value.length === 0) {
      rows.forEach(function (row) {
        row.style.display = "";
      })
    } else {
      var pred = function (str) {
        return value.every(function (needle) {
          return str.includes(needle);
        });
      };

      rows.forEach(function (row) {
        var haystack = row.dataset.memberHaystack || "";
        row.style.display = pred(haystack) ? "" : "none";
      });
    }
  });
});
