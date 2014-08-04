
//// newToggle and toggle are paired

function newToggle(on) {
	return {
		on: !!on
	};
}

function toggle(t) {
	return t.on = !t.on;
}

function expensive() {
  var i;
  document.getElementById("expensive").innerHTML = "expensive start";
  for (i = 0; i < 100000000; i++) {
    3+Math.random();
  }
  document.getElementById("expensive").innerHTML = "expensive end";
}

////  this is meant to be separate
//// an alternative to newToggle()/toggle()

function newToggleObj(on) {
	return {
		toggle: function() {
			return on = !on;
		}
	};
}
