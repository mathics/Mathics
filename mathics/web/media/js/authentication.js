var authenticated = false;

var loginReason;
var loginNext;

function showLogin(reason, next) {
	$('passwordSent').hide();
	if (reason) {
		$('loginReason').show();
		$('loginReason').setText(reason);
	} else
		$('loginReason').hide();
	showPopup($('login'));
	loginNext = next;
}

function cancelLogin() {
	hidePopup();
}

function onLogin(username) {
	$('notAuthenticated').hide();
	$('authenticated').show();
	$('username').setText(username);
	authenticated = true;
}

function onLogout() {
	$('authenticated').hide();
	$('notAuthenticated').show();
	$('username').setText('');
	authenticated = false;	
}

function login() {
	submitForm('loginForm', '/ajax/login/', function(response) {
		var result = response.result;
		var email = response.form.values.email;
		if (result == 'ok') {
			onLogin(email);
			cancelLogin();
			if (loginNext)
				loginNext();
		} else {
			$('passwordEmail').setText(email);
			$('passwordSent').show();
			$('id_password').activate(); 
		}
	})
}

function logout() {
	new Ajax.Request('/ajax/logout/', {
		method: 'post',
		onSuccess: onLogout
	});
}

function requireLogin(reason, onLogin) {
	loginReason = reason;
	if (REQUIRE_LOGIN && !authenticated)
		showLogin(reason, onLogin);
	else
		onLogin();
}

function checkLogin(response) {
	if (REQUIRE_LOGIN && response.requireLogin) {
		onLogout();
		hidePopup();
		showLogin(loginReason, loginNext);
		return false;
	} else
		return true;
}
