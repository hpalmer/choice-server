/**
 *  @global {object} userlib
 *  @name userlib.listUsers
 **/
/**
 * @global {object} attrlib
 * @name {function} attrlib.getAttributesOf
 * @name {function} attrlib.getAttributeValuesOf
 */
/**
 * @global {object} filelib
 * @name {function} filelib.newPrintWriter
 */
var obj;

/**
 * Check whether this is just retrieving help for this script. Help is returned
 * in a structured form, so that it can be used by a program.
 */
if (((args.length === 1) && (args[0] === '--help')) || (args.length === 0)) {
    obj = {
        // Zero status is the normal return for help.
        status: 0,
        // The description is an array of lines of text. The caller may choose
        // to reformat the text.
        description: [
            'This script lists the users in a group and retrieves any metadata',
            'attributes associated with each user. It returns all this information',
            'as an object which includes a "users" field. The value of that field',
            'is an array of objects, one per user, with all the information about',
            'that user. Optionally this script will write the user information to',
            'a specified CSV file. If the specified output file already exists, it',
            'is replaced.'
        ],
        // An array of descriptions of arguments
        arguments: [
            {
                short: 'group path',
                long: 'the full path of a user group',
                type: 'string',
                required: true
            },
            {
                short: 'output path',
                long: 'the path of an output file',
                type: 'string',
                required: false
            }
        ]
    }
}
else {
    obj = (args.length > 1) ? main(args[0], args[1]) : main(args[0]);
}

function main(group, outpath) {
    var obj = userlib.listUsers(group, false);
    if (obj.status !== 1) {
        throw new Error(obj.msg);
    }
    var attrnames = [];
    var maplist = obj.users;
    for (var i = 0; i < maplist.length; ++i) {
        var umap = maplist[i];
        umap.attrs = [];
        var uattrs = attrlib.getAttributesOf(umap.path);
        if (uattrs.length) {
            uattrs = attrlib.getAttributeValuesOf(umap.path, uattrs);
            umap.attrs = uattrs;
            for (var j = 0; j < uattrs.length; ++j) {
                var aname = uattrs[j].name;
                if (attrnames.indexOf(aname) < 0) {
                    attrnames.push(aname);
                }
            }
        }
    }

    obj = {
        status: 1,
        attributes: attrnames.sort(function (n1, n2) {
            n1 = n1.substring(n1.lastIndexOf('/') + 1);
            n2 = n2.substring(n2.lastIndexOf('/') + 1);
            return n1.localeCompare(n2);
        }),
        users: maplist.sort(function (umap1, umap2) {
            return umap1.name.localeCompare(umap2.name);

        })
    };

    if (outpath) {
        try {
            var writer = filelib.newPrintWriter(outpath);
            var columns = Object.keys(obj.users[0]);
            columns.splice(columns.indexOf('attrs'), 1);
            var line = columns.map(valueToField).join(',');
            if (obj.attributes.length > 0) {
                line += ',' + obj.attributes.map(function(an) {
                        return valueToField(an.substring(an.lastIndexOf('/') + 1));
                    }).join(',');
            }
            writer.println(line);
            for (i = 0; i < obj.users.length; ++i) {
                umap = obj.users[i];
                line = '';
                for (j = 0; j < columns.length; ++j) {
                    if (j !== 0) {
                        line += ',';
                    }
                    if ((columns[j] === 'crtime') || (columns[j] === 'mtime')) {
                        line += valueToField(new Date(umap[columns[j]]), true);
                    }
                    else {
                        line += valueToField(umap[columns[j]]);
                    }
                }
                var uatt = {};
                for (j = 0; j < umap.attrs.length; ++j) {
                    uatt[umap.attrs[j].name] = umap.attrs[j].value;
                }
                for (j = 0; j < obj.attributes.length; ++j) {
                    line += ',' + valueToField(uatt[obj.attributes[j]]);
                }
                writer.println(line);
            }
            writer.close();
            obj.write = { status: 1 };
        }
        catch (ex) {
            obj.write = { status: -1, msg: ex.toString() };
        }
    }

    return obj;
}

function valueToField(aval, isDate) {
    if ((aval === undefined) || (aval === null)) {
        return '';
    }
    if (isDate) {
        return '"' + aval.toLocaleString() + '"';
    }
    if (typeof aval === 'string') {
        return '"' + aval + '"';
    }
    if (typeof aval === 'number') {
        return String(aval);
    }
    if (typeof aval === 'boolean') {
        return (aval) ? '1' : '0';
    }
    if (typeof aval === 'object') {
        if (aval.length !== undefined) {
            return '"' + aval.map(function(v) { return String(v); }).join(' ') + '"';
        }
    }
    return '';
}

obj;
