/**
 * Copyright © 2017 The Board of Trustees of The Leland Stanford Junior University.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
/**
 * This is used to reset a user password, for a user whose email address is known.
 * It sends an email to the user with a link to a password reset page. The password
 * reset page runs this script again, providing a secret from the emailed link,
 * and a new password.
 *
 * The location of the reset password page can be customized, or else it defaults
 * to the value of DefaultResetPage.
 *
 * @author Howard Palmer
 * Created by Hep on 3/23/2017.
 */
/**
 * @external args
 * @type {*[]}
 */
/**
 * @external SCRIPT_PATH
 * @type {string}
 */
/**
 * @name userlib
 * @type {Object}
 * @property {function} checkUser
 * @property {function} getSession
 */
/**
 * @name userlib#getSession
 * @return {Object} session information object
 */
/**
 * @name userlib#checkUser
 * @param {string} username
 * @param {string} password
 * @param {string} group
 * @return {Object}
 */
/**
 * @name filelib
 * @type {Object}
 * @property {function} lookup
 * @property {function} sendMail
 * @property {function} queueScript
 */
/**
 * @name httplib
 * @type {Object}
 * @property {function} getHostAndPath
 */
var DefaultResetPage = '/resetPassword.html';
var Queue = 'ResetPassword';
var EmailRegex = /^[a-z0-9._%+-]+@[a-z0-9.-]+\.[a-z]{2,3}$/;

if (args.length < 1) {
    throw new Error('insufficient arguments');
}
/**
 * @typedef {Object} QueuedScriptInfo
 * @property {String} event name of event causing this script to run
 * @property {*[]} eventArgs arguments of the event
 * @property {String} queue the queue name
 * @property {String} filename the queued filename
 * @property {String} script the path to the script
 * @property {String} fileArgs JSON-encoded arguments from original queuing
 * @property {String} userPrincipal path to the principal who queued the file
 * @property {String} filePrincipal path to the principal who owns the script
 * @property {String} scriptOwner
 * @property {bool} asOwner true if the script runs as the script owner
 */
/**
 * Requeue this queued script.
 *
 * Assumes that this script was executed as a queued script, so that args[0] is
 * an object containing QueuedScriptInfo.
 * @return {bool} true if an existing queue element was replaced
 */
function requeue() {
    /** @type QueuedScriptInfo */
    var obj = args[0];
    return filelib.queueScript(Queue, obj.filename, obj.script, obj.fileArgs, true, obj.asOwner);
}

function main() {
    //
    // This script is invoked on three occasions:
    //     1. To validate user and group, determine the user's email address,
    //        and send a reset password link
    //     2. To validate a reset password link
    //     3. To actually reset the password to a new password
    //
    // Step 1 creates a queued script, namely this script. Step 2 occurs when
    // the script is dequeued. If Step 2 is successful, the client prompts for
    // the new password and runs this script again as Step 3.
    //
    if ((args.length > 0) && (typeof args[0] === 'string')) {
        // Normal client invocation of this script: Step 1
        var username = args[0];
        if (args.length > 1) {
            var group = args[1];
            if (args.length > 2) {
                var page = args[2] || DefaultResetPage;
            }
        }
        return initialRequest(username, group, page);
    }

    if (args.length === 0) {
        throw new Error('invalid resetPassword invocation');
    }

    // Otherwise this script was queued, and now due to an event, has been dequeued and run.
    /** @type QueuedScriptInfo */
    var obj = args[0];
    if (obj.event === 'checkSecret') {
        // Step 2
        if (obj.eventArgs && obj.eventArgs.length > 1) {
            return checkSecret(obj, obj.eventArgs[0], obj.eventArgs[1]);
        }
        throw new Error('missing event arguments');
    }

    if (obj.event === 'newPassword') {
        // Step 3
        if (obj.eventArgs && obj.eventArgs.length > 1) {
            return handleReset(obj, obj.eventArgs[0], obj.eventArgs[1]);
        }
        throw new Error('missing arguments for newPassword event');
    }

    throw new Error('unknown event: ' + obj.event);
}

/**
 * This is the initial request.
 *      Determine the group of the user, using the login group from
 *          current session if no group is specified
 *      Validate the given username as a username in the group
 *      Check whether the username looks like an email address
 *      Try to get the email attribute of the user if the username is not an email address.
 *      Generate a secret string
 *      Queue this script in the ResetPassword queue
 *      Send an email to the user with a link containing the email address and the secret,
 *          and referencing the given page or DefaultResetPage
 *
 * @param {String} username the username
 * @param {String} [group] optional group path
 * @param {String} page path to the reset password page
 * @return {Object}
 *     @property {String} code 'ok' if successful, errors are 'nouser' or 'noemail'
 *     @property {String} email if code is 'ok' the user's email address
 */
function initialRequest(username, group, page) {
    var email = null;
    var result = { code: 'broken' };
    if (!group) {
        group = getLoginGroup();
    }
    obj = grouplib.findUser(group, username, true);
    if (obj === null) {
        result.code = 'nouser';
        return result;
    }
    if (obj.status === 1 && obj.mimetype === 'choice/user') {
        // User is in group
        // Is the user banned?
        if (!obj.isLoginAllowed) {
            result.code = 'banned';
            return result;
        }
        // Does the user authenticate with a password?
        if (obj.oauthOnly) {
            result.code = 'oauth';
            return result;
        }
        // Try to get an email address
        if (EmailRegex.test(username)) {
            email = username;
        }
        else if (EmailRegex.test(obj.email)) {
            email = obj.email;
        }
        else {
            result.code = 'noemail';
            return result;
        }
    }
    else {
        if (obj.msg && (obj.msg.lastIndexOf('string matching regex', 0) === 0)) {
            result.code = 'baduser';
            return result;
        }
        throw new Error(obj.msg);
    }

    // Create the secret
    var secret = randomString(16, '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ');

    // Must match what getFileArgs() expects
    var fileArgs = [username, group, page, secret, email];

    var update = filelib.queueScript(Queue, email, SCRIPT_PATH, JSON.stringify(fileArgs), true, true);

    if (!sendEmail(page, email, secret)) {
        throw new Error('sendEmail(' + page + ', ' + email + ', <secret>)');
    }
    result.code = 'ok';
    result.email = email;
    return result;
}

/**
 * Check the email and secret from a reset password link.
 *
 * @param {QueuedScriptInfo} obj
 * @param {String} email email address from link query parameter
 * @param {String} secret secret from link query parameter
 * @return {String} 'ok' if both match, otherwise 'badlink'
 */
function checkSecret(obj, email, secret) {
    var code = 'fileargs';
    var fileArgs = getFileArgs(obj);
    if ((fileArgs.email === email) && (fileArgs.secret === secret)) {
        requeue();
        code = 'ok';
    }
    else {
        requeue();
        code = 'badlink';
    }
    return { code: code, username: fileArgs.username };
}

function handleReset(obj, password, secret) {
    var fileArgs = getFileArgs(obj);
    if (fileArgs.secret === secret) {
        if (userlib.resetPassword(fileArgs.username, password, fileArgs.group)) {
            return 'ok';
        }
    }
    return 'badlink';
}

function getLoginGroup() {
    var group = null;
    var obj = userlib.getSession();
    if (obj.status === 1) {
        if (obj.logintype === 'none') {
            // The user is not logged in, so use the current login group
            group = obj.logingroup;
        }
        else {
            // The user is logged in. Any of the user's groups where the username matches will do,
            // since they all reference the same user identity.
            for (var i = 0; i < obj.groups.length; ++i) {
                if (username === obj.groups[i].member) {
                    group = obj.groups[i].paths[0];
                    break;
                }
            }
        }
    }
    if (!group) {
        throw new Error('unable to determine the user\'s group')
    }
    return group;
}

function sendEmail(page, email, secret) {
    // Use my mailer if testing locally. Otherwise use the default mailer.
    var mailer = (getHostname() === 'localhost') ? '/home/howard/Pacbell' : null;

    var urlPrefix = httplib.getHostAndPath();
    // If the page starts with the context path (e.g. '/fschoice'), strip it off,
    // since the urlPrefix already includes it.
    var contextPath = httplib.getContextPath();
    if (contextPath && page.lastIndexOf(contextPath, 0) === 0) {
        page = page.substring(contextPath.length);
    }
    var robj = filelib.sendMail({
        to: email,
        subject: 'Password reset',
        content: 'A request has been made to reset your password on ' + getHostname() + '.\n' +
                 'You can set a new password by following this link:\n\n' +
                 '    ' + urlPrefix + page + '?id=' + secret + '&email=' + email + '\n\n' +
                 'If you did not make this request, then someone may be attempting\n' +
                 'to break into your account.'
    }, mailer);
    if (robj.status !== 1) {
        throw new Error('sendMail: ' + robj.msg);
    }
    return true;
}

function getHostname() {
    var hp = httplib.getHostAndPath();
    var match = hp.match(/^https?:\/\/([^:\/]+)/);
    return match[1];
}

function randomString(length, chars) {
    var result = '';
    for (var i = length; i > 0; --i) result += chars[Math.floor(Math.random() * chars.length)];
    return result;
}

function getFileArgs(obj) {
    var jsonFileArgs = obj.fileArgs;
    if (jsonFileArgs) {
        var fileArgs = JSON.parse(jsonFileArgs);
        if (fileArgs && fileArgs.length === 5) {
            return {
                username: fileArgs[0],
                group: fileArgs[1],
                page: fileArgs[2],
                secret: fileArgs[3],
                email: fileArgs[4]
            };
        }
    }
    throw new Error('file arguments are incorrect');
}

var result = main();

result;
