;;; project-init-skeletons.el --- project skeletons -*- lexical-binding: t -*-
;;; Commentary:
;; Project skeleton data
;; Hints: a ! indicates that the variable mandates a project structure
;;; Code:
(require 'project-init-settings)
(require 'project-init-utils)
(eval-when-compile (require 'project-init-deffile))
(require 'f)


;; Each file takes a project name as an argument and returns
;; a list: the car is the filename and the cadr is what is
;; contained in the file. Deffile encapsulates this pattern.

;; Idris

;; (deffile doooooot
;;   :takes-project-name t
;;   :docstring "Make doot with PROJECT-NAME."
;;   :filename "doot.txt"
;;   :contents ("a" "b" "c"))

(deffile project-init--gitignore-idris
  :takes-project-name nil
  :docstring ("Ignore ibc files. Should be put at the bottom of the gitignore.")
  :filename ".gitignore"
  :contents ("*.ibc"))

(deffile project-init--gitignore-idris!
  :takes-project-name nil
  :docstring ("Ignore binary files."
              "Mandatest this project structure: "
              "project-name/project/src/project-name.idr"
              "where binary files end up in project/."
              "PROJECT-NAME is not used.")
  :filename ".gitignore"
  :contents ("!project/*.ipkg"
             "!project/src"
             "*.ibc"))

(deffile project-init--ipkg-idris!
  :takes-project-name t
  :docstring ("Create an executable with the name PROJECT-NAME."
              "Place this executable in the project/ directory."
              "Mandates the same project structure as"
              "PROJECT-INIT--GITIGNORE-IDRIS!, and uses the name of the project"
              "as the name of the executable.")
  :filename "build.ipkg"
  :contents ((format "package %s" project-name)
             ""
             (format "modules = %s" project-name)
             ""
             "sourcedir = src"
             ""
             (format "main = %s" project-name)
             ""
             (format "executable = %s" project-name)))


(deffile project-init--helloworld-idris
  :takes-project-name t
  :docstring ("A helloworld template for idris."
              "PROJECT-NAME is used as part of the filename.")
  :filename (format "%s.idr" project-name)
  :contents ("module Main"
             "total main : IO ()"
             "main = putStrLn \"Hello World!\""))

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
(deffile project-init--gitignore-java
  :takes-project-name nil 
  :docstring ("The gitignore for Gradle Java projects.")
  :filename ".gitignore"
  :contents ("# Created by https://www.gitignore.io/api/java,gradle"
             "# Edited to respect the 80 char rule"
             "### Java ###"
             "# Compiled class file"
             "*.class"
             ""
             "# Log file"
             "*.log"
             ""
             "# BlueJ files"
             "*.ctxt"
             ""
             "# Mobile Tools for Java (J2ME)"
             ".mtj.tmp/"
             ""
             "# Package Files #"
             "*.jar"
             "*.war"
             "*.ear"
             "*.zip"
             "*.tar.gz"
             "*.rar"
             ""
             "# virtual machine crash logs, see"
             "# http://www.java.com/en/download/help/error_hotspot.xml"
             "hs_err_pid*"
             ""
             "### Gradle ###"
             ".gradle"
             "**/build/"
             ""
             "# Ignore Gradle GUI config"
             "gradle-app.setting"
             ""
             "# Avoid ignoring Gradle wrapper jar file "
             "# (.jar files are usually ignored)"
             "!gradle-wrapper.jar"
             ""
             "# Cache of project"
             ".gradletasknamecache"
             ""
             "# # Work around "
             "# https://youtrack.jetbrains.com/issue/IDEA-116898"
             "# gradle/wrapper/gradle-wrapper.properties"
             ""
             ""
             "# End of https://www.gitignore.io/api/java,gradle"
             ""))

(deffile project-init--build-java-bin!
  :takes-project-name t
  :docstring ("Using PROJECT-NAME, make a build.gradle for binaries."
              "Will need to be updated in versions etc from time to time.")
  :filename "build.gradle"
  :contents ("plugins {"
             "    id 'com.github.johnrengelman.shadow' version '2.0.4'"
             "    id 'java'"
             "}"
             ""
             "repositories {"
             "    jcenter()"
             "}"
             ""
             "dependencies {"
             "    compile 'org.slf4j:slf4j-api:1.7.21'"
             "    testCompile 'junit:junit:4.12'"
             "}"
             ""
             "jar {"
             "    manifest {"
             (format "        attributes 'Main-Class': '%s'" project-name)
             "    }"
             "}"
             ""))

(deffile project-init--helloworld-java-bin!
  :takes-project-name t
  :docstring ("Using PROJECT-NAME, make a helloworld program.")
  :filename (format "%s.java" project-name)
  :contents ("/**"
             " * The main class of the program."
             (format " * @author %s" project-init-author-name)
             " */"
             (format "public class %s {" project-name)
             "    /**"
             "     * The entry point of the program."
             "     * @param args Command line arguments. Not used."
             "     */"
             "    public static void main(String[] args) {"
             "        System.out.println(\"Hello world!\");"
             "    }"
             "}"))


(deffile project-init--test-java-bin!
  :takes-project-name t
  :docstring ("A test file to make gradle happy.
Uses PROJECT-NAME to generate the filename.")
  :filename (format "%sTest.java" project-name)
  :contents ("import org.junit.Test;"
             "import static org.junit.Assert.*;"
             "/**"
             " * Test the project."
             (format " * @author %s" project-init-author-name)
             " */"
             (format "public class %sTest {" project-name)
             "    @Test"
             "    public void test() {"
             "    }"
             "}"))

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

;; Chicken Scheme
(defun project-init--gitignore-egg (&optional _)
  '("*.so
*.import.*" ))

(provide 'project-init-skeletons)
;;; project-init-skeletons.el ends here
