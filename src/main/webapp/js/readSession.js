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
            'This script reads information from a session folder, which may be',
            'specified by file path or file id. It returns the session information',
            'as an object.'
        ],
        // An array of descriptions of arguments
        arguments: [
            {
                short: 'file path',
                long: 'the full path of a session folder',
                type: 'string',
                required: false
            },
            {
                short: 'file id',
                long: 'the file id of a session folder',
                type: 'number',
                required: false
            }
        ]
    }
}
else {
    obj = main(args[0]);
}

function main(pathOrId) {
    var id = Number(pathOrId);
    var path;
    if (isNaN(id)) {
        id = undefined;
        path = pathOrId;
    }
    if (!path) {
        var idobj = filelib.lookupById(id);
        if (idobj.status === 1) {
            path = idobj.path;
        }
        else {
            return idobj;
        }
    }
    var rsobj = filelib.readSession(path);
    return rsobj;
}

obj;
