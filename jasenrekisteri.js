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

  function tagModifier() {
    var wrappers = document.querySelectorAll('div.row[data-jrek-person-tag]');

    console.info("tagModifier", wrappers);
  }

  function loginForm() {
    var loginForm = document.querySelector("#login-form");
    if (!loginForm) return;

    var loginUser = loginForm.querySelector("#login-user");
    var loginPass = loginForm.querySelector("#login-pass");
    var loginButton = loginForm.querySelector("#login-button");

    if (!loginUser || !loginPass || !loginButton) {
      console.error("Broken login form");
      return;
    }

    console.info("Found login form");

    // Enable button when non empty texts
    var u$ = menrva.source(loginUser.value);
    var p$ = menrva.source(loginPass.value);
    var ok$ = menrva.source(true);

    function bindChange(source, el) {
      function cb() {
        menrva.transaction()
          .set(source, el.value)
          .set(ok$, true)
          .commit();
      }

      el.addEventListener("keyup", cb);
      el.addEventListener("change", cb);
    }

    bindChange(u$, loginUser);
    bindChange(p$, loginPass);

    menrva.combine(u$, p$, function (u, p) {
      return u !== "" && p !== "";
    }).onValue(function (b) {
      loginButton.disabled = !b;
    });

    ok$.onValue(function (ok) {
      loginForm.className = ok ? "callout primary" : "callout warning";
    });

    // click
    loginButton.addEventListener("click", function () {
      var u = loginUser.value;
      var p = loginPass.value;

      login(u, p).then(function (res) {
        console.info("/login returned", res);
        if (res) {
          document.cookie = "JREK_SESSION_ID=" + res;
          location.reload();
        } else {
          menrva.transaction().set(ok$, false).commit();
        }
      });
    });
  }

  memberFilter();
  tagToggler();
  tagModifier();
  loginForm();

  // jsonFetch

  // login, TODO: remove
  function login(u, p) {
    var url = "/login";

    var headers = new Headers();
    headers.append("Accept", "application/json");
    headers.append("Content-Type", "application/json");

    var opts = {
      method: "POST",
      headers: headers,
      body: JSON.stringify({
        user: u,
        pass: p,
      }),
    };

    return fetch(url, opts)
      .then(function (res) {
        console.debug("response", url, res.ok);
        return res.json();
      });
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
