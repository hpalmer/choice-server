var grouppath, obj, i;
if (args.length === 0) {
    obj = userlib.getSession();
    grouppath = obj.user.path;
    i = grouppath.lastIndexOf('/');
    if (i >= 0) {
        grouppath = grouppath.substring(0, i);
    }
}
else {
    grouppath = args[0];
}
obj = filelib.list('/System/Sessions/active', 'choice/session');
if (obj.status === 1) {
    var slist = obj.files;
    var wholist = [];
    for (i = 0; i < slist.length; ++i) {
        obj = filelib.readSession(slist[i].path);
        var usergroup = obj.userPath;
        var username = usergroup;
        var j = usergroup.lastIndexOf('/');
        if (j >= 0) {
            username = usergroup.substring(j + 1);
            usergroup = usergroup.substring(0, j);
        }
        if (usergroup === grouppath) {
            wholist.push(username);
        }
    }
}

[grouppath, wholist];
