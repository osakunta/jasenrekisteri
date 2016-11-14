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
    var checkboxes = $$('input[type=checkbox]');

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

  function tagModifier() {
    $$("div.row[data-jrek-person-tag]").forEach(function (wrapper) {
      var input = $("input[type=text]", wrapper);
      var addButton = $("button[data-jrek-action=add]", wrapper);
      var removeButton = $("button[data-jrek-action=remove]", wrapper);

      var memberId = wrapper.dataset.jrekPersonTag;

      var input$ = menrva.source("");
      var cb = function () {
        menrva.transaction()
          .set(input$, input.value.trim())
          .commit();
      };
      input.addEventListener("keyup", cb);
      input.addEventListener("change", cb);

      input$.onValue(function (value) {
        addButton.disabled = value === "";
        removeButton.disabled = value === "";
      });

      addButton.addEventListener("click", function () {
        commandAddTag(memberId, input.value.trim());
        input.value = "";
        cb();
      });

      removeButton.addEventListener("click", function () {
        commandRemoveTag(memberId, input.value.trim());
        input.value = "";
        cb();
      });
    });
  }

  memberFilter();
  tagToggler();
  tagModifier();

  // Utilities

  function $(selector, el) {
    el = el || document;
    return el.querySelector(selector, el);
  }

  function $$(selector, el) {
    el = el || document;
    var res = el.querySelectorAll(selector, el);
    return Array.prototype.slice.call(res);
  }

  // Commands

  function command(cmd) {
    var url = "/command";

    var headers = new Headers();
    headers.append("Accept", "application/json");
    headers.append("Content-Type", "application/json");

    var opts = {
      method: "POST",
      headers: headers,
      credentials: "same-origin",
      body: JSON.stringify(cmd),
    };

    return fetch(url, opts)
      .then(function (res) {
        console.debug("response", url, res.ok);
        return res.json();
      });
  }

  function commandAddTag(memberId, tagName) {
    assert(_.isString(memberId), "memberId should be string");
    assert(_.isString(tagName), "memberId should be string");

    console.info("command add-tag", memberId, tagName);
    return command({
      type: "add-tag",
      memberId: memberId,
      tagName: tagName,
    });
  }

  function commandRemoveTag(memberId, tagName) {
    assert(_.isString(memberId), "memberId should be string");
    assert(_.isString(tagName), "memberId should be string");

    console.info("command remove-tag", memberId, tagName);
    return command({
      type: "remove-tag",
      memberId: memberId,
      tagName: tagName,
    });
  }

  function assert(cond, msg) {
    if (!cond) {
      console.error(msg);
      throw new Error(msg);
    }
  }
});
