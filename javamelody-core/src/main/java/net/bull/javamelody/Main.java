/*
 * The MIT License
 *
 * Copyright (c) 2008-2011, Sun Microsystems, Inc., Alan Harder, Jerome Lacoste, Kohsuke Kawaguchi,
 * bap2000, CloudBees, Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package net.bull.javamelody;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.JarURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.MissingResourceException;
import java.util.jar.JarFile;
import java.util.jar.Manifest;

/**
 * Launcher class for stand-alone execution of JavaMelody Collector Server as
 * {@code java -jar javamelody-collector-server.war}.
 *
 * <p>On a high-level architectural note, this class is intended to be a very thin wrapper whose
 * primary purpose is to extract Winstone and delegate to Winstone's own initialization mechanism.
 * The logic in this class should only perform JavaMelody-specific argument and environment validation
 * and JavaMelody-specific Winstone customization prior to delegating to Winstone.
 *
 * <p>In particular, managing the logging subsystem is completely delegated to Winstone, and this
 * class should neither assume that logging has been initialized nor take advantage of the logging
 * subsystem. In the event that this class needs to print information to the user, it should do so
 * via the standard output (stdout) and standard error (stderr) streams rather than via the logging
 * subsystem. Such messages should generally be avoided except for fatal scenarios, such as an
 * inappropriate Java Virtual Machine (JVM) or some other serious failure that would preclude
 * starting Winstone.
 *
 * @author Kohsuke Kawaguchi, extracted (and simplified) from Jenkins by Emeric Vernat
 * 	licence MIT (alias X11, donc compatible Apache-2.0)
 */
@SuppressWarnings("all")
public final class Main {
	private Main() {
		super();
	}

	private static boolean hasArgument(String argument, String[] args) {
		for (String arg : args) {
			if (argument.equals(arg)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Méthode main appelée par la JVM.
	 * @param args String[]
	 * @throws Exception e
	 */
	public static void main(String[] args) throws IllegalAccessException {
		final boolean java21OrLater = "21".compareTo(System.getProperty("java.version")) < 0;
		if(!java21OrLater) {
			System.out.println("Minimum Java required version for JavaMelody Collector Server is 21, not " + System.getProperty("java.version"));
		}

		//Allows to pass arguments through stdin to "hide" sensitive parameters like httpsKeyStorePassword
		//to achieve this use --paramsFromStdIn
		if (hasArgument("--paramsFromStdIn", args)) {
			System.out.println("--paramsFromStdIn detected. Parameters are going to be read from stdin. Other parameters passed directly will be ignored.");
			String argsInStdIn;
			try {
				argsInStdIn = new String(System.in.readNBytes(131072), StandardCharsets.UTF_8).trim();
			} catch (IOException e) {
				throw new UncheckedIOException(e);
			}
			args = argsInStdIn.split(" +");
		}
		// If someone just wants to know the version, print it out as soon as possible, with no extraneous info.
		// This makes it easier to grab the version from a script
		final List<String> arguments = new ArrayList<>(List.of(args));
		if (arguments.contains("--version")) {
			System.out.println(getVersion("?"));
			return;
		}

		File extractedFilesFolder = null;
		for (String arg : args) {
			if (arg.startsWith("--extractedFilesFolder=")) {
				extractedFilesFolder = new File(arg.substring("--extractedFilesFolder=".length()));
				if (!extractedFilesFolder.isDirectory()) {
					System.err.println("The extractedFilesFolder value is not a directory. Ignoring.");
					extractedFilesFolder = null;
				}
			}
		}


		// this is so that JRobin can work nicely even if we are launched as a daemon
		System.setProperty("java.awt.headless", "true");

		File me = whoAmI(extractedFilesFolder);
		System.out.println("Running from: " + me);
		System.setProperty("executable-war", me.getAbsolutePath());  // remember the location so that we can access it from within webapp

		// figure out the arguments
		trimOffOurOptions(arguments);
		arguments.add(0, "--warfile=" + me.getAbsolutePath());

		// only do a cleanup if you set the extractedFilesFolder property.
		if (extractedFilesFolder != null) {
			deleteContentsFromFolder(extractedFilesFolder, "winstone.*\\.jar");
		}

		// put winstone jar in a file system so that we can load jars from there
		File tmpJar = extractFromJar("/winstone-jenkins.jar", "winstone", ".jar", extractedFilesFolder);
		tmpJar.deleteOnExit();

		// clean up any previously extracted copy, since
		// winstone doesn't do so and that causes problems when newer version of JavaMelody Collector Server
		// is deployed.
		File tempFile;
		try {
			tempFile = File.createTempFile("dummy", "dummy");
		} catch (IOException e) {
			throw new UncheckedIOException(e);
		}
		deleteWinstoneTempContents(new File(tempFile.getParent(), "winstone/" + me.getName()));
		if (!tempFile.delete()) {
			System.err.println("Failed to delete temporary file: " + tempFile);
		}

		// locate the Winstone launcher
		ClassLoader cl;
		try {
			cl = new URLClassLoader("JavaMelody Collector Server Main ClassLoader",
					new URL[] {tmpJar.toURI().toURL()}, ClassLoader.getSystemClassLoader());
		} catch (MalformedURLException e) {
			throw new UncheckedIOException(e);
		}
		Class<?> launcher;
		Method mainMethod;
		try {
			launcher = cl.loadClass("winstone.Launcher");
			mainMethod = launcher.getMethod("main", String[].class);
		} catch (ClassNotFoundException | NoSuchMethodException e) {
			throw new AssertionError(e);
		}

		// override the usage screen
		Field usage;
		try {
			usage = launcher.getField("USAGE");
		} catch (NoSuchFieldException e) {
			throw new AssertionError(e);
		}
		usage.set(null, "Jenkins Automation Server Engine " + getVersion("") + "\n" +
				"Usage: java -jar jenkins.war [--option=value] [--option=value]\n" +
				"\n" +
				"Options:\n" +
				"   --extractedFilesFolder   = folder where extracted files are to be located. Default is the temp folder\n" +
				"   --paramsFromStdIn        = Read parameters from standard input (stdin)\n" +
				"   --version                = Print version to standard output (stdout) and exit\n" +
				"{OPTIONS}");

		// run
		Thread.currentThread().setContextClassLoader(cl);
		try {
			mainMethod.invoke(null, new Object[] {arguments.toArray(new String[0])});
		} catch (InvocationTargetException e) {
			Throwable t = e.getCause();
			if (t instanceof RuntimeException) {
				throw (RuntimeException) t;
			} else if (t instanceof IOException) {
				throw new UncheckedIOException((IOException) t);
			} else if (t instanceof Exception) {
				throw new RuntimeException(t);
			} else if (t instanceof Error) {
				throw (Error) t;
			} else {
				throw new RuntimeException(e);
			}
		}
	}

	private static void trimOffOurOptions(List<String> arguments) {
		arguments.removeIf(arg -> arg.startsWith("--extractedFilesFolder"));
	}

	/**
	 * Figures out the version from the manifest.
	 */
	private static String getVersion(String fallback) {
	  try {
		Enumeration<URL> manifests = Main.class.getClassLoader().getResources("META-INF/MANIFEST.MF");
		while (manifests.hasMoreElements()) {
			URL res = manifests.nextElement();
			Manifest manifest = new Manifest(res.openStream());
			String v = manifest.getMainAttributes().getValue("Implementation-Version");
			if (v != null) {
				return v;
			}
		}
	  } catch (IOException e) {
		throw new UncheckedIOException(e);
	  }
	  return fallback;
	}

	private static boolean hasOption(List<String> args, String prefix) {
		for (String s : args) {
			if (s.startsWith(prefix)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Figures out the URL of {@code jenkins.war}.
	 */
	public static File whoAmI(File directory) {
		// JNLP returns the URL where the jar was originally placed (like http://jenkins-ci.org/...)
		// not the local cached file. So we need a rather round about approach to get to
		// the local file name.
		// There is no portable way to find where the locally cached copy
		// of javamelody-collector-server.war/jar is; JDK 6 is too smart. (See JENKINS-2326.)
		try {
			URL classFile = Main.class.getClassLoader().getResource("javamelody/Main.class");
			JarFile jf = ((JarURLConnection) classFile.openConnection()).getJarFile();
			return new File(jf.getName());
		} catch (Exception x) {
			System.err.println("ZipFile.name trick did not work, using fallback: " + x);
		}
		File myself;
		try {
			myself = File.createTempFile("javamelody-collector-server", ".jar", directory);
		} catch (IOException e) {
			throw new UncheckedIOException(e);
		}
		myself.deleteOnExit();
		try (InputStream is = Main.class.getProtectionDomain().getCodeSource().getLocation().openStream();
			 OutputStream os = new FileOutputStream(myself)) {
			is.transferTo(os);
		} catch (IOException e) {
			throw new UncheckedIOException(e);
		}
		return myself;
	}

	/**
	 * Extract a resource from jar, mark it for deletion upon exit, and return its location.
	 */
	private static File extractFromJar(String resource, String fileName, String suffix, File directory) {
		URL res = Main.class.getResource(resource);
		if (res == null) {
			throw new MissingResourceException("Unable to find the resource: " + resource, Main.class.getName(), resource);
		}

		// put this jar in a file system so that we can load jars from there
		File tmp;
		try {
			tmp = File.createTempFile(fileName, suffix, directory);
		} catch (IOException e) {
			String tmpdir = directory == null ? System.getProperty("java.io.tmpdir") : directory.getAbsolutePath();
			throw new UncheckedIOException("JavaMelody Collector Server failed to create a temporary file in " + tmpdir + ": " + e, e);
		}
		try (InputStream is = res.openStream(); OutputStream os = new FileOutputStream(tmp)) {
			is.transferTo(os);
		} catch (IOException e) {
			throw new UncheckedIOException(e);
		}
		tmp.deleteOnExit();
		return tmp;
	}

	/**
	 * Search contents to delete in a folder that match with some patterns.
	 *
	 * @param folder folder where the contents are.
	 * @param patterns patterns that identifies the contents to search.
	 */
	private static void deleteContentsFromFolder(File folder, final String... patterns) {
		File[] files = folder.listFiles();

		if (files != null) {
			for (File file : files) {
				for (String pattern : patterns) {
					if (file.getName().matches(pattern)) {
						deleteWinstoneTempContents(file);
					}
				}
			}
		}
	}

	private static void deleteWinstoneTempContents(File file) {
		if (!file.exists()) {
			return;
		}
		if (file.isDirectory()) {
			File[] files = file.listFiles();
			if (files != null) { // be defensive
				for (File value : files) {
					deleteWinstoneTempContents(value);
				}
			}
		}
		if (!file.delete()) {
			System.err.println("Failed to delete temporary Winstone file: " + file);
		}
	}
}
