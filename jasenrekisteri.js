/*
   How to use fetch:
   https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch
*/

document.addEventListener("DOMContentLoaded", function () {
  console.info("Initialising foundation");
  jQuery(document).foundation();

  console.info("Initialising jasenrekisteri");

  var eventNames = [ "click", "change" ];
  var eachWithKey = _.each.convert({ 'cap': false });

  memberFilter();
  tagToggler();
  tagModifier();
  searchButtons();
  personEdit();
  personCreate();
  searchBox();
  var overlay = addOverlay();

  // "components"

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
      console.info("Initialising member tag edit for", memberId);

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

  function searchButtons() {
    var searchEl = $("div.jrek-search");
    if (!searchEl) return;
    var inputEl = $("input[name=query]", searchEl);
    if (!inputEl) return;

    $$("button[data-jrek-search-string]").forEach(function (button) {
      var q = button.dataset.jrekSearchString;

      button.addEventListener("click", function () {
        inputEl.value = q;
      });
    });
  }

  function personEdit() {
    var formEl = $("div[data-jrek-member-edit]");
    if (!formEl) return;
    var submitButton = $("button[data-jrek-action=submit]", formEl);
    if (!submitButton) return;

    var memberId = formEl.dataset.jrekMemberEdit;
    console.info("Initialising member edit form for", memberId);

    var fields = $$("input[data-jrek-field-name]");

    // button toggler
    var cb = function () {
      var hasChanges = fields.some(function (f) {
        return f.dataset.jrekFieldValue !== f.value;
      });

      submitButton.disabled = !hasChanges;
    }

    cb();
    fields.forEach(function (f) {
      f.addEventListener("change", cb);
      f.addEventListener("keyup", cb);
    });

    submitButton.addEventListener("click", function () {
      var edit = {};
      var hasChanges = false;
      fields.forEach(function (f) {
        var n = f.dataset.jrekFieldName;
        var o = f.dataset.jrekFieldValue;
        var v = f.value;

        if (o !== v) {
          edit[n] = v;
          hasChanges = true;
        } else {
          edit[n] = null;
        }
      });

      if (hasChanges) {
        commandMemberEdit(memberId, edit);
      }
    });
  }

  function personCreate() {
    var formEl = $("div[data-jrek-member-new]");
    if (!formEl) return;
    var submitButton = $("button[data-jrek-action=submit]", formEl);
    if (!submitButton) return;

    console.info("Initialising new member form");

    var fields = $$("input[data-jrek-field-name]");

    submitButton.addEventListener("click", function () {
      var edit = {};
      var hasChanges = false;
      fields.forEach(function (f) {
        var n = f.dataset.jrekFieldName;
        var v = f.value.trim();
        if (v !== "") {
          edit[n] = v;
          hasChanges = true;
        }
      });

      if (hasChanges) {
        commandMemberNew(edit);
      }
    });
  }

  function addOverlay() {
    var overlay = document.createElement("DIV");
    overlay.className = "jrek-overlay";
    overlay.style.display = "none";
    document.body.appendChild(overlay);

    var message = dom("div", { className: "jrek-message callout primary" }, [ "placeholder-message" ]);

    overlay.appendChild(dom("div", { className: "row" }, [
      dom("div", { className: "large-12 columns" },  [ message ]),
    ]));

    return {
      overlay: overlay,
      message: message,
    };
  }

  function searchBox() {
    var box = $(".top-bar .search");
    if (!box) return;

    console.info("Initialising search box");

    var fetchData = _.once(function () {
      var headers = new Headers();
      headers.append("Accept", "application/json");

      var opts = {
        method: "GET",
        headers: headers,
        credentials: "same-origin",
      };

      return fetch("/search-data", opts)
        .then(function (res) {
          return res.json();
        });
    });

    function typeToEl(type) {
      if (type === "tag") {
        return dom("i", [ "tägi: " ]);
      } else {
        return dom("i", [ "jäsen: " ]);
      }
    }

    var ac = jQuery(box).autocomplete({
      minLength: 3,

      source: function (req, res) {
        var term = req.term.toLowerCase();
        fetchData().then(function (x) {
          // check that items match the term
          x = x.filter(function (item) {
            return item.value.toLowerCase().includes(term);
          });

          res(x);
        });
      },

      select: function (ev, ui) {
        console.debug("Selected", ui);
        location.href = ui.item.href;
      },
    });

    ac.data("ui-autocomplete")._renderItem = function (ul, item) {
      var li = dom("li", {
        className: "ui-menu-item"
      },
      [ dom("span", { className: "ui-menu-item-wrapper" }, [
          typeToEl(item.type), item.label
        ])
      ]);
      var li = jQuery(li);
      li.focus(function () {
        console.log("focus");
      });
      ul.append(li);
      return li;
    };
  }

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

  function assert(cond, msg) {
    if (!cond) {
      console.error(msg);
      throw new Error(msg);
    }
  }

  function dom(elName, args, children) {
    if (_.isArray(args)) {
      children = args;
      args = {};
    }

    children = children || [];
    args = args || {};

    var el = document.createElement(elName);

    eachWithKey(function (value, key) {
      if (eventNames.indexOf(key) === -1) {
        el[key] = value;
      } else {
        el.addEventListener(key, value);
      }
    }, args);

    children.forEach(function (child) {
      if (_.isString(child)) {
        el.appendChild(domText(child));
      } else {
        el.appendChild(child);
      }
    });

    return el;
  }

  function domText(t) {
    return document.createTextNode(t);
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
      })
      .catch(function (exc) {
        overlay.message.classList.add("alert");
        overlay.message.classList.remove("primary");
        overlay.message.innerHTML = "Tapahtui virhe!<br />" + exc
        overlay.overlay.style.display = "";
        throw exc;
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

  function commandMemberEdit(memberId, edit) {
    assert(_.isString(memberId), "memberId should be string");
    assert(_.isPlainObject(edit), "edit should be a plain object");

    console.info("command member-edit", memberId, edit);
    return command({
      type: "member-edit",
      memberId: memberId,
      edit: edit,
    })
    .then(function () {
      overlay.message.innerText = "";
      overlay.message.appendChild(dom("div", [
        dom("p", [ "Sivua pitää päivittä, jotta muutokset tulevat näkyviin." ]),
        dom("button", {
          className: "button",
          click: function () {
            location.reload();
          }
        }, [ "Päivitä" ]),
      ]));
      overlay.overlay.style.display = "";
    });
  }

  function commandMemberNew(edit) {
    assert(_.isPlainObject(edit), "edit should be a plain object");

    console.info("command member-new", edit);
    return command({
      type: "member-new",
      edit: edit,
    })
    .then(function (res) {
      console.log(res);

      overlay.message.innerText = "";
      overlay.message.appendChild(dom("div", [
        dom("p", [ "Käytäjä luotu. Siirry sen sivulle." ]),
        dom("button", {
          className: "button",
          click: function () {
            location.href = "/member/" + res;
          }
        }, [ "Siirry" ]),
      ]));
      overlay.overlay.style.display = "";
    });
  }
});
