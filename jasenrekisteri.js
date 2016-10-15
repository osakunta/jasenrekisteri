/*
   How to use fetch:
   https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch
*/

document.addEventListener("DOMContentLoaded", function () {
  console.info("Initialising jasenrekisteri");

  function memberFilter () {
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
  }

  function tagToggler() {
    var checkboxes = document.querySelectorAll('input[type=checkbox]');

    checkboxes.forEach(function (checkbox) {
      if (!checkbox.dataset.tag) return;

      var json = JSON.parse(checkbox.dataset.tag);

      checkbox.addEventListener("change", function () {
        if (checkbox.checked) {
          commandAddTag(json.memberId, json.tagName);
        } else {
          commandRemoveTag(json.memberId, json.tagName);
        }
      });
    });
  }

  memberFilter();
  tagToggler();

  // Commands

  function command(cmd) {
    var url = "/command";

    var headers = new Headers();
    headers.append("Accept", "application/json");
    headers.append("Content-Type", "application/json");

    var opts = {
      method: "POST",
      headers: headers,
      body: JSON.stringify(cmd),
    };

    return fetch(url, opts)
      .then(function (res) {
        console.debug("response", url, res.ok);
      });
  }

  function commandAddTag(memberId, tagName) {
    return command({
      type: "add-tag",
      memberId: memberId,
      tagName: tagName,
    });
  }

  function commandRemoveTag(memberId, tagName) {
    return command({
      type: "remove-tag",
      memberId: memberId,
      tagName: tagName,
    });
  }
});
