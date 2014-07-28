
function newToggle(on) {
	return {
		on: !!on
	};
}

function toggle(t) {
	return t.on = !t.on;
}

function newToggleObj(on) {
	return {
		toggle: function() {
			return on = !on;
		}
	};
}
