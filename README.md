### Stanford GSE AAALab Choice Server

This is a
[Scala](https://www.scala-lang.org/)/[Lift](https://liftweb.net/)-based
content server that was developed at the Stanford
Graduate School of Education [AAALab](https://aaalab.stanford.edu/). The purpose
of the server is to serve our web-based games, called Choicelets, and collect
data from the game sessions. The server enables researchers to playback user
game sessions, and also to collect statistics from the session.

The server is designed as a general-purpose content server, where the content
is stored in an object-oriented filesystem, which we'll call the Choice
filesystem. Various types of system content are represented as special file
types. This includes for example, users, user groups, and access control policies,
roles, and rights. Content is protected by a role-based access control system,
with rights which can be specific to particular file types. File metadata
attributes also can be defined, enabling values for these attributes to be
assigned to a file.

The server supports the creation of multiple, distinct user groups, and a user
group contain a hierarchy of subgroups. Creation of a user group is controlled
by an access right, which can be assigned to any user. This feature was
motivated by a need to have separate user groups for different experiments. The
server also supports server-side scripting using Lua or JavaScript. Scripts
can be run with the rights of the user who invokes them, or with the rights
of the user who owns the script file, depending on the access rights.

The server has been under development for about seven years. The Scala
language was chosen because it appeared to be a better vehicle for developing
JVM code than Java. The Lift platform was chosen because it was the only
Scala server platform available at the time.

This is the basic server without much infrastructure. The server is built
with:

```sbt clean package```

which produces ```target/scala-2.12/choice-server.war```. We normally deploy
that on Tomcat 7 or 8, with MySql as the database server. The server stores
all file metadata, including filenames, in the SQL database. The actual file
data is stored in anonymous numbered files, which are normally stored in a
```ChoiceData``` folder at the root of a Tomcat instance.

When the server is started under Tomcat, it has a ```boot.html``` file,
which provides the mechanisms to register the first user, and to upload
other content. The first user to register becomes a system administrator.

The (soon-to-appear) ```choice-desktop``` repository contains one candidate
for the initial upload to the server. It provides a window manager in a
browser window (implemented in part with [Scala.js](https://www.scala-js.org/)),
and various tools,
including a file browser, the Ace editor, a file upload tool, and a command-line
"terminal".

What is missing at this point is a lot of documentation that will be needed
to configure and use the server. Hopefully that will be appearing soon.
