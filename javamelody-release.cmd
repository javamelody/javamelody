@echo off
setlocal

:: javamelody release
:: check variables and tools
if not defined releaseVersion (
	echo Variable releaseVersion is not set. Use set releaseVersion=x.y.z
	exit /B
)
if not defined developmentVersion (
	echo Variable developmentVersion is not set. Use set developmentVersion=x.y.z-SNAPSHOT
	exit /B
)
if not defined java_home (
	echo Variable java_home is not set. Use set java_home=C:\Program Files\Java\jdk1.8.0_112 for example
	exit /B
)

where /q mvn.cmd
IF ERRORLEVEL 1 (
    ECHO mvn.cmd is missing. Ensure Maven bin directory is in your PATH.
    EXIT /B
)
where /q git.exe
IF ERRORLEVEL 1 (
    ECHO git.exe is missing. Ensure git bin directory is in your PATH.
    EXIT /B
)
where /q gpg.exe
IF ERRORLEVEL 1 (
    ECHO gpg.exe is missing. Ensure gpg pub directory is in your PATH.
    EXIT /B
)

echo javamelody release:
echo releaseVersion=%releaseVersion%
echo developmentVersion=%developmentVersion%
if /I "%dryRun%"=="true" echo dryRun=%dryRun%
echo.
call mvn -version || exit /B
git --version || exit /B

:: clean before
if exist javamelody-release rmdir /s /q javamelody-release
mkdir javamelody-release
cd javamelody-release

git clone https://github.com/javamelody/javamelody
cd javamelody

:: javamelody-core: mvn release
echo.
echo javamelody-core ...
cd javamelody-core
call mvn clean || exit /B
if /I NOT "%dryRun%" == "true" (
call mvn release:prepare release:perform -Dtag=javamelody-core-%releaseVersion% -DreleaseVersion=%releaseVersion% -DdevelopmentVersion=%developmentVersion% || exit /B
call mvn versions:set -DgenerateBackupPoms=false -DnewVersion=%releaseVersion% || exit /B
call mvn source:jar javadoc:jar -DskipTests || exit /B
) else (
call mvn versions:set -DgenerateBackupPoms=false -DnewVersion=%releaseVersion% || exit /B
call mvn install source:jar javadoc:jar -DskipTests || exit /B
)

:: package collector server: put release version in javamelody-collector-server/pom.xml and clean install
echo.
echo javamelody-collector-server ...
cd ../javamelody-collector-server
call mvn versions:set -DgenerateBackupPoms=false -DnewVersion=%releaseVersion% || exit /B
call mvn clean install || exit /B

:: package spring boot starter: put release version in javamelody-spring-boot-starter/pom.xml and clean install
echo.
echo javamelody-spring-boot-starter ...
cd ../javamelody-spring-boot-starter
call mvn versions:set -DgenerateBackupPoms=false -DnewVersion=%releaseVersion% || exit /B
call mvn clean install || exit /B

:: deploy to https://oss.sonatype.org
echo.
echo deploy to https://oss.sonatype.org
cd ../javamelody-core
call mvn gpg:sign-and-deploy-file -Durl=https://oss.sonatype.org/service/local/staging/deploy/maven2/ -DrepositoryId=sonatype-nexus-staging -DpomFile=pom.xml -Dfile=target/javamelody-core-%releaseVersion%.jar || exit /B
call mvn gpg:sign-and-deploy-file -Durl=https://oss.sonatype.org/service/local/staging/deploy/maven2/ -DrepositoryId=sonatype-nexus-staging -DpomFile=pom.xml -Dfile=target/javamelody-core-%releaseVersion%-sources.jar -Dclassifier=sources || exit /B
call mvn gpg:sign-and-deploy-file -Durl=https://oss.sonatype.org/service/local/staging/deploy/maven2/ -DrepositoryId=sonatype-nexus-staging -DpomFile=pom.xml -Dfile=target/javamelody-core-%releaseVersion%-javadoc.jar -Dclassifier=javadoc || exit /B
cd ../javamelody-collector-server
call mvn gpg:sign-and-deploy-file -Durl=https://oss.sonatype.org/service/local/staging/deploy/maven2/ -DrepositoryId=sonatype-nexus-staging -DpomFile=pom.xml -Dfile=target/javamelody-collector-server-%releaseVersion%.war || exit /B
cd ../javamelody-spring-boot-starter
call mvn gpg:sign-and-deploy-file -Durl=https://oss.sonatype.org/service/local/staging/deploy/maven2/ -DrepositoryId=sonatype-nexus-staging -DpomFile=pom.xml -Dfile=target/javamelody-spring-boot-starter-%releaseVersion%.jar || exit /B
call mvn gpg:sign-and-deploy-file -Durl=https://oss.sonatype.org/service/local/staging/deploy/maven2/ -DrepositoryId=sonatype-nexus-staging -DpomFile=pom.xml -Dfile=target/javamelody-spring-boot-starter-%releaseVersion%-sources.jar -Dclassifier=sources || exit /B
call mvn gpg:sign-and-deploy-file -Durl=https://oss.sonatype.org/service/local/staging/deploy/maven2/ -DrepositoryId=sonatype-nexus-staging -DpomFile=pom.xml -Dfile=target/javamelody-spring-boot-starter-%releaseVersion%-javadoc.jar -Dclassifier=javadoc || exit /B

:: create javamelody release in github
echo create javamelody release in github
cd ../javamelody-core
(
echo **Downloads:**
echo - See [Release Notes v%releaseVersion%](../../wiki/ReleaseNotes#%releaseVersion:.=%^) and [User guide](../../wiki/UserGuide^) for install.
echo - If you use Maven, add a [dependency](../../wiki/UserGuide#dependencies^) in the pom.xml of your webapp:
echo.
echo ```xml
echo         ^<dependency^>
echo                 ^<groupId^>net.bull.javamelody^</groupId^>
echo                 ^<artifactId^>javamelody-core^</artifactId^>
echo                 ^<version^>%releaseVersion%^</version^>
echo         ^</dependency^>
echo ```
echo - [javamelody-core-%releaseVersion%.jar](../../releases/download/javamelody-core-%releaseVersion%/javamelody-core-%releaseVersion%.jar^) : Jar for integration in a webapp
echo - [javamelody-collector-server-%releaseVersion%.war](../../releases/download/javamelody-core-%releaseVersion%/javamelody-collector-server-%releaseVersion%.war^) : War of the optional collect server, not needed in most use cases
echo.
echo **Plugins:**
echo - [JIRA / Confluence / Bamboo / Bitbucket](../../wiki/AtlassianPlugin^)
echo - [Jenkins](http://wiki.jenkins-ci.org/display/JENKINS/Monitoring^)
echo - [Liferay](https://github.com/javamelody/liferay-javamelody^)
echo - [Alfresco](https://github.com/javamelody/alfresco-javamelody^)
echo - [Sonar](https://github.com/javamelody/sonar-javamelody^)
echo - [Grails](http://www.grails.org/plugin/grails-melody^)
) > javamelody-release-notes.txt

call mvn com.ragedunicorn.tools.maven:github-release-maven-plugin:github-release -Ddraft=%dryRun% -Downer=javamelody -Drepository=javamelody -Dserver=github-release -DtagName=javamelody-core-%releaseVersion% -Dname="JavaMelody v%releaseVersion%" -DtargetCommitish=master -DreleaseNotes=javamelody-release-notes.txt -Dassets=target/javamelody-core-%releaseVersion%.jar,../javamelody-collector-server/target/javamelody-collector-server-%releaseVersion%.war || exit /B
del javamelody-release-notes.txt
cd ..

:: increment versions in pom.xml files
echo.
echo development version
call mvn versions:set -DgenerateBackupPoms=false -DnewVersion=%developmentVersion% || exit /B
cd javamelody-core
call mvn versions:set -DgenerateBackupPoms=false -DnewVersion=%developmentVersion% || exit /B
cd ../javamelody-collector-server
call mvn versions:set -DgenerateBackupPoms=false -DnewVersion=%developmentVersion% || exit /B
cd ../javamelody-spring-boot-starter
call mvn versions:set -DgenerateBackupPoms=false -DnewVersion=%developmentVersion% || exit /B
cd ../javamelody-for-standalone
call mvn versions:set -DgenerateBackupPoms=false -DnewVersion=%developmentVersion% || exit /B
cd ../javamelody-for-spring-boot
call mvn versions:set -DgenerateBackupPoms=false -DnewVersion=%developmentVersion% || exit /B
cd ../javamelody-offline-viewer
call mvn versions:set -DgenerateBackupPoms=false -DnewVersion=%developmentVersion% || exit /B
cd ../javamelody-objectfactory
call mvn versions:set -DgenerateBackupPoms=false -DnewVersion=%developmentVersion% || exit /B
if /I NOT "%dryRun%" == "true" (
git commit -a -m %developmentVersion% || exit /B
git push || exit /B
)

cd ../..

:: monitoring-plugin: increment javamelody-core version in pom.xml and mvn release
echo.
echo monitoring-plugin ...
git clone https://github.com/jenkinsci/monitoring-plugin
cd monitoring-plugin
call mvn versions:use-dep-version -Dincludes=net.bull.javamelody:javamelody-core -DdepVersion=%releaseVersion% -DgenerateBackupPoms=false || exit /B
if /I NOT "%dryRun%" == "true" (
git commit -a -m javamelody-core-%releaseVersion% || exit /B
git push || exit /B
call mvn release:prepare release:perform -Dtag=monitoring-%releaseVersion% -DreleaseVersion=%releaseVersion% -DdevelopmentVersion=%developmentVersion% || exit /B
) else (
call mvn clean install || exit /B
)
cd ..

:: jira-confluence-javamelody: increment version, clean install and github release
echo.
echo jira-confluence-javamelody ...
git clone https://github.com/javamelody/jira-confluence-javamelody
cd jira-confluence-javamelody
call mvn versions:set -DgenerateBackupPoms=false -DnewVersion=%releaseVersion% || exit /B
call mvn clean install || exit /B
if /I NOT "%dryRun%" == "true" (
git commit -a -m %releaseVersion% || exit /B
git push || exit /B
)
call mvn com.ragedunicorn.tools.maven:github-release-maven-plugin:github-release -Ddraft=%dryRun% -Downer=javamelody -Drepository=jira-confluence-javamelody -Dserver=github-release -DtagName=%releaseVersion% -Dname=%releaseVersion% -DtargetCommitish=master -Dbody="Release notes: https://github.com/javamelody/javamelody/wiki/ReleaseNotes#%releaseVersion:.=%" -Dassets=target/jira-confluence-javamelody-%releaseVersion%.jar || exit /B
cd ..

:: sonar-javamelody: increment version, clean install and github release
echo.
echo sonar-javamelody ...
git clone https://github.com/javamelody/sonar-javamelody
cd sonar-javamelody
call mvn versions:set -DgenerateBackupPoms=false -DnewVersion=%releaseVersion% || exit /B
call mvn clean install || exit /B
if /I NOT "%dryRun%" == "true" (
git commit -a -m %releaseVersion% || exit /B
git push || exit /B
)
call mvn com.ragedunicorn.tools.maven:github-release-maven-plugin:github-release -Ddraft=%dryRun% -Downer=javamelody -Drepository=sonar-javamelody -Dserver=github-release -DtagName=%releaseVersion% -Dname=%releaseVersion% -DtargetCommitish=master -Dbody="Release notes: https://github.com/javamelody/javamelody/wiki/ReleaseNotes#%releaseVersion:.=%" -Dassets=target/sonar-javamelody-plugin-%releaseVersion%.jar || exit /B
cd ..

:: liferay-javamelody: increment version, clean install and github release
echo.
echo liferay-javamelody ...
git clone https://github.com/javamelody/liferay-javamelody
cd liferay-javamelody
call mvn versions:set -DgenerateBackupPoms=false -DnewVersion=%releaseVersion%.0 || exit /B
call mvn versions:use-dep-version -Dincludes=net.bull.javamelody:javamelody-core -DdepVersion=%releaseVersion% -DgenerateBackupPoms=false || exit /B
if /I NOT "%dryRun%" == "true" (
git commit -a -m %releaseVersion% || exit /B
git push || exit /B
)
call mvn clean install || exit /B
call mvn com.ragedunicorn.tools.maven:github-release-maven-plugin:github-release -Ddraft=%dryRun% -Downer=javamelody -Drepository=liferay-javamelody -Dserver=github-release -DtagName=%releaseVersion% -Dname=%releaseVersion% -DtargetCommitish=master -Dbody="Release notes: https://github.com/javamelody/javamelody/wiki/ReleaseNotes#%releaseVersion:.=%" -Dassets=target/liferay-javamelody-hook-%releaseVersion%.0.war || exit /B
cd ..

:: alfresco-javamelody: increment version, clean install, github release and deploy to https://oss.sonatype.org
echo.
echo alfresco-javamelody ...
git clone https://github.com/javamelody/alfresco-javamelody
cd alfresco-javamelody
call mvn versions:set -DgenerateBackupPoms=false -DnewVersion=%releaseVersion% || exit /B
if /I NOT "%dryRun%" == "true" (
git commit -a -m %releaseVersion% || exit /B
git push || exit /B
)
call mvn clean install || exit /B
call mvn com.ragedunicorn.tools.maven:github-release-maven-plugin:github-release -Ddraft=%dryRun% -Downer=javamelody -Drepository=alfresco-javamelody -Dserver=github-release -DtagName=%releaseVersion% -Dname=%releaseVersion% -DtargetCommitish=master -Dbody="Release notes: https://github.com/javamelody/javamelody/wiki/ReleaseNotes#%releaseVersion:.=%" -Dassets=target/alfresco-javamelody-addon-%releaseVersion%.amp || exit /B
call mvn gpg:sign-and-deploy-file -Durl=https://oss.sonatype.org/service/local/staging/deploy/maven2/ -DrepositoryId=sonatype-nexus-staging -DpomFile=pom.xml -Dfile=target/alfresco-javamelody-addon-%releaseVersion%.amp || exit /B
cd ..

:: clean-up after
cd ..
rmdir /s /q javamelody-release

:: done
echo javamelody release success
exit /B
