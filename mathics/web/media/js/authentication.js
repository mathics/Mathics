/**
    Mathics: a general-purpose computer algebra system
    Copyright (C) 2011-2013 The Mathics Team

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
**/

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
