### Stanford GSE AAALab Choice Server

#### Introduction and Features

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
group can contain a hierarchy of subgroups. Creation of a user group is controlled
by an access right, which can be assigned to any user. This feature was
motivated by a need to have separate user groups for different experiments. The
server also supports server-side scripting using Lua or JavaScript. Scripts
can be run with the rights of the user who invokes them, or with the rights
of the user who owns the script file, depending on the access rights.

The server has been under development for about seven years. The Scala
language was chosen because it appeared to be a better vehicle for developing
JVM code than Java. The Lift platform was chosen because it was the only
Scala server platform available at the time.

#### Prerequisites

The main prerequisites are a servlet container and a SQL database. At AAALab
the servlet container is Apache Tomcat 7 or 8, and the database is MySQL.
Other servlet containers such as Jetty, and databases such as PostgreSQL
should work as well, but have never been tested.

Some features of the server also have prerequisites. For example, support
for Google login requires an application client id and secret from Google
to be configured. And the ability for the server to send email requires
the configuration of an SMTP server connection.

This repository is just the content server itself. When it runs initially
there are no users, and very little initial content. There is a page,
boot.html, where the initial system administrator user can be registered,
and content in the form of .zip or .jar files can be uploaded.

The [choice-desktop](https://github.com/hpalmer/choice-desktop) repository provides a platform
for managing the server, including a file browser and file upload tools,
the [Ace](https://ace.c9.io/) editor, and a terminal emulation with a
command-line interpreter. Since currently there is no other way to manage
the server, this also is prerequisite initial content for using the server.
It typically would be uploaded from the boot.html page.

#### Building the server

The server is built with:

```sbt clean package```

which produces ```target/scala-2.12/choice-server.war```. This currently
produces some warning messages, which seem to be inconsequential.

#### Deploying the server

For Tomcat, deployment typically consists of the following steps:

  * Create the SQL database to be used by the server for storing its
  content metadata. For example, with MySQL that might be accomplished
  with:
  
      create database choice default charset utf8mb4;
 
  * Create a user identity for the server to use to access the database
  that was just created. Give this user all access rights to that database.

  * Stop Tomcat
  
  * Copy the choice-server.war file produced by the build to the Tomcat
  webapps folder. The name of the .war file in webapps determines the
  content root of the server. At AAALab it is typically named fschoice.war,
  which means that the server content URLS always start with "/fschoice".
  To eliminate the initial component of the URL, copy the .war file to
  webapps as ROOT.war. This typically would be appropriate when the
  Tomcat instance is not hosting any other servlets.
  
  * Configure the server (see details) below. This includes designating
  a host folder where the data files for the server's content will be
  stored. The server will attempt to create this folder if it does not
  exist, but this may fail if Tomcat is running under a user identity
  without the necessary permissions. By default, this folder is named
  "ChoiceData", and is located in Tomcat's ${CATALINA_BASE} folder.
  
  * Start Tomcat. The server will take some time to start, as it creates
  the database schema and loads the initial content files from the server
  .war file into its filesystem. This should not take more than a couple
  of minutes. If it does, check the Tomcat logs for errors.
  
  * Browse to the server's boot.html page. For example, if your server
  is on localhost port 8080, and you named the .war file "fschoice.war",
  then the URL would be:
  
      http://localhost:8080/fschoice/boot.html
  
  * Register the initial administrator user. The first user to register
  becomes the administrator, and logins for other users are disabled.
  Using the choice-desktop platform, the administrator can create other
  users, and designate other administrators. Logins can be enabled, and
  self-registration for normal users can be enabled if desired.
  
  * Upload the initial content as a .zip or .jar file. Multiple such
  content files can be uploaded, but usually one of them will be the
  choice-desktop platform.
  
  * Navigate to the choice-desktop platform and use to set up access
  control policies on your content, or to upload additional content.
  Note that access to almost all content is initially restricted to
  administrators, so setting policies to allow read access to normal
  users is usually essential.
  
  * The details of server administration will require a full document
  that has not yet been written. For early experimenters, it is recommended
  to get the choice-desktop working, and then type "help" in its
  terminal tool.
  
#### Server Configuration

Server configuration files can be included in the server .war file, or
in the case of Tomcat deployment, stored under the ${CATALINA_BASE}/conf
folder. In the server source tree there are configuration files in:

  src/main/resources/props
  src/main/webapp/META-INF
  src/main/webapp/WEB-INF
  
If you modify these files in the source tree, they will be included in the
server .war file produced by the build. However, if you are going to be
updating the server source, you may encounter conflicts in these files if
the default configuration changes.

Alternatively you can override the default configuration with files placed
in:

  ${CATALINA_BASE}/conf/Catalina/localhost
  
Assuming your .war file is named fschoice.war, an fschoice.xml file in that
folder will override the META-INF/context.xml file in the .war file, and
files in:

  ${CATALINA_BASE}/conf/Catalina/localhost/fschoice/
  
will override the configuration files of resources/props in the .war file.

The most essential, and potentially only configuration needed is to specify
the server database connection. This is in the META-INF/context.xml file
(or alternatively in the example above, ${CATALINA_BASE}/conf/Catalina/localhost/fschoice.xml).
This file actually is read by Tomcat, and is used to create a JNDI resource
for the database, which is subsequently accessed by the server. The most
important items are the database URL, username, and password.

The property files of src/main/resources/props are read by the Lift platform.
Lift has a "run.mode" property which is used to select which files to use.
If run.mode is not set (the default), then "default.props" and "default.logback.xml"
are used. The property, "choice.HostDataFolder", specifies where the server will
store the data files for the content in its filesystem. These are stored as
numbered .dat files, as the naming hierarchy for server content is part of the
metadata stored in the SQL database.

The choice.HostDataFolder is ${CATALINA_BASE}/ChoiceData by default, but this
is another item you may wish to change. Wherever the folder is, the server
will try to create it if it doesn't already exist.
