;;; project-init-skeletons.el --- project skeletons -*- lexical-binding: t -*-
;;; Commentary:
;; Project skeleton data
;; Hints: a ! indicates that the variable mandates a project structure
;; Each takes a project name as an argument and returns
;; a list: the car is the filename and the cadr is what is contained in the file
;;; Code:
(require 'project-init-settings)
(require 'project-init-utils)
(require 'f)

;; Idris
(defun project-init--gitignore-idris (&optional _)
  "Ignore ibc files.  Should be put at the bottom of the gitignore."
  '(".gitignore" "*.ibc"))

(defun project-init--gitignore-idris! (&optional _)
  "Ignore binary files.
Mandates this project structure: project-name/project/src/project-name.idr,
where binary files end up in project/.
PROJECT-NAME is not used"
  (let ((the-pair (project-init--gitignore-idris)))
    (list (car the-pair)
          (concat "project/*
!project/*.ipkg
!project/src
"         (cadr the-pair)))))

(defun project-init--ipkg-idris! (project-name)
  "Create an executable with the name PROJECT-NAME in the project/ directory.
Mandates the same project structure as PROJECT-INIT--GITIGNORE-IDRIS!,
and uses the name of the project as the name of the executable."
  (list "build.ipkg"  (let ((pn project-name))
                        (format "package %s

modules = %s

sourcedir = src

main = %s

executable = %s
"                      pn pn pn pn))))

(defun project-init--helloworld-idris (project-name)
  "A helloworld template for idris.
PROJECT-NAME is not used."
  (list (format "%s.idr" project-name) "module Main
total main : IO ()
main = putStrLn \"Hello world!\""))

;; Skeleton layouts
;; Organized like this:
;; toplevel dir: ((file filename) (file filename) (file filename)
;;                ("directory" (file filename) (file filename) (file filename)
;;                 ("directory" (file filename))
;;                 ("directory" (file filename)))) etc
;; where file is a function that takes the project name and returns a string
;; The toplevel directory is assumed to be the name of the project
(defvar project-init--idris-layout
  '(project-init--gitignore-idris project-init--helloworld-idris)
  "Skeleton layout for idris that does not mandate a project structure.")

(defvar project-init--idris-layout!
  '(project-init--gitignore-idris!
    ("project" project-init--ipkg-idris!
     ("src" project-init--helloworld-idris)))
  "Skeleton layout for idris that does mandate a project structure.")

;; Java (Gradle)
(defun project-init--gitignore-java (&optional _)
  "The gitignore for Gradle Java projects."
  '(".gitignore" "# Created by https://www.gitignore.io/api/java,gradle
# Edited to respect the 80 char rule
### Java ###
# Compiled class file
*.class

# Log file
*.log

# BlueJ files
*.ctxt

# Mobile Tools for Java (J2ME)
.mtj.tmp/

# Package Files #
*.jar
*.war
*.ear
*.zip
*.tar.gz
*.rar

# virtual machine crash logs, see
# http://www.java.com/en/download/help/error_hotspot.xml
hs_err_pid*

### Gradle ###
.gradle
**/build/

# Ignore Gradle GUI config
gradle-app.setting

# Avoid ignoring Gradle wrapper jar file (.jar files are usually ignored)
!gradle-wrapper.jar

# Cache of project
.gradletasknamecache

# # Work around https://youtrack.jetbrains.com/issue/IDEA-116898
# gradle/wrapper/gradle-wrapper.properties


# End of https://www.gitignore.io/api/java,gradle
"))

(defun project-init--build-java-bin! (project-name)
  "Using PROJECT-NAME, make a build.gradle for binaries.
Will need to be updated in versions etc from time to time."
  (list "build.gradle" (format "plugins {
    id 'com.github.johnrengelman.shadow' version '2.0.4'
    id 'java'
}

repositories {
    jcenter()
}

dependencies {
    compile 'org.slf4j:slf4j-api:1.7.21'
    testCompile 'junit:junit:4.12'
}

jar {
    manifest {
	attributes 'Main-Class': '%s'
    }
}
"                      project-name)))

(defun project-init--helloworld-java-bin! (project-name)
  "With the filename PROJECT-NAME, make a helloworld file."
  (list (concat project-name ".java")
        (format "/**
 * The main class of the program.
 * @author %s
 */
public class %s {
    /**
     * The entry point of the program.
     * @param args Command line arguments. Not used.
     */
    public static void main(String[] args) {
        System.out.println(\"Hello world!\");
    }
}"      project-init-author-name project-name
)))


(defun project-init--test-java-bin! (&optional _)
  "A test file to make gradle happy."
  (list (concat project-name "Test" ".java")
        (format "import org.junit.Test;
import static org.junit.Assert.*;
/**
 * Test the project.
 * @author %s
 */
public class %sTest {
    @Test
    public void test() {
    }
}"      project-init-author-name project-name)))

(defvar project-init--java-layout!
  '(project-init--gitignore-java project-init--build-java-bin!
    ("src" ("main"
            ("java" project-init--helloworld-java-bin!))
           ("test"
            ("java" project-init--test-java-bin!)))))

(defun project-init--java-cleanup! (project-name)
  "Clean up files like build.gradle, Library.java in PROJECT-NAME."
  (mapc 'f-delete (list (project-init--expand-file-names project-name
                                                         "build.gradle")
                        (project-init--expand-file-names project-name
                                                         "src"
                                                         "main"
                                                         "java"
                                                         "Library.java")
                        (project-init--expand-file-names project-name
                                                         "src"
                                                         "test"
                                                         "java"
                                                         "LibraryTest.java"))))

(provide 'project-init-skeletons)
;;; project-init-skeletons.el ends here
