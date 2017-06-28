/*
The MIT License

Copyright (c) 2004-, Kohsuke Kawaguchi, Sun Microsystems, Inc., and a number of other of contributers

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
 */
package net.bull.javamelody;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.JarURLConnection;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.jar.JarFile;
import java.util.zip.ZipFile;

/**
 * Launcher class for stand-alone execution of JavaMelody Monitoring as
 * <tt>java -jar javamelody.war</tt>.
 *
 * @author Kohsuke Kawaguchi, extracted (and simplified) from Jenkins by Emeric Vernat
 * 	licence MIT (alias X11, donc compatible ASL)
 */
@SuppressWarnings("all")
public final class Main {
	private Main() {
		super();
	}

	/**
	 * Méthode main appelée par la JVM.
	 * @param args String[]
	 * @throws Exception e
	 */
	public static void main(String[] args) throws Exception {
		// if we need to daemonize, do it first,
		// (Non : http://fr.wikipedia.org/wiki/Nohup devrait suffire en général)
		//		for (int i = 0; i < args.length; i++) {
		//			if (args[i].startsWith("--daemon")) {
		//				// load the daemonization code
		//				ClassLoader cl = new URLClassLoader(
		//						new URL[] {
		//								extractFromJar("WEB-INF/lib/jna-3.0.9.jar", "jna", "jar").toURI()
		//										.toURL(),
		//								extractFromJar("WEB-INF/lib/akuma-1.1.jar", "akuma", "jar").toURI()
		//										.toURL(), });
		//				Class $daemon = cl.loadClass("com.sun.akuma.Daemon");
		//				Object daemon = $daemon.newInstance();
		//
		//				// tell the user that we'll be starting as a daemon.
		//				Method isDaemonized = $daemon.getMethod("isDaemonized", new Class[] {});
		//				if (!((Boolean) isDaemonized.invoke(daemon, new Object[0])).booleanValue()) {
		//					System.out.println("Forking into background to run as a daemon.");
		//					if (!hasLogOption(args))
		//						System.out.println("Use --logfile to redirect output to a file");
		//				}
		//
		//				Method m = $daemon.getMethod("all", new Class[] { boolean.class });
		//				m.invoke(daemon, new Object[] { Boolean.TRUE });
		//			}
		//		}

		// en commentaire car bug JENKINS-3272 (sous windows)
		// if the output should be redirect to a file, do it now
		//		for (int i = 0; i < args.length; i++) {
		//			if (args[i].startsWith("--logfile=")) {
		//				final LogFileOutputStream los = new LogFileOutputStream(new File(args[i]
		//						.substring("--logfile=".length())));
		//				final PrintStream ps = new PrintStream(los);
		//				System.setOut(ps);
		//				System.setErr(ps);
		//				// don't let winstone see this
		//				final List myArgs = new ArrayList(Arrays.asList(args));
		//				myArgs.remove(i);
		//				args = (String[]) myArgs.toArray(new String[myArgs.size()]);
		//				break;
		//			}
		//		}

		// this is so that JRobin can work nicely even if we are launched as a daemon
		System.setProperty("java.awt.headless", "true");

		final File me = whoAmI();
		getSystemOutputStream().println("Running from: " + me);
		System.setProperty("executable-war", me.getAbsolutePath()); // remember the location so that we can access it from within webapp

		// put winstone jar in a file system so that we can load jars from there
		final File tmpJar = extractFromJar("/winstone-jenkins.jar", "winstone", "jar");

		// clean up any previously extracted copy, since
		// winstone doesn't do so and that causes problems when newer version is deployed.
		final File tempFile = File.createTempFile("dummy", "dummy");
		deleteContents(new File(tempFile.getParent(), "winstone/" + me.getName()));
		deleteFile(tempFile);

		// locate the Winstone launcher
		final ClassLoader cl = new URLClassLoader(new URL[] { tmpJar.toURI().toURL() });
		final Class<?> launcher = cl.loadClass("winstone.Launcher");
		final Method mainMethod = launcher.getMethod("main", new Class<?>[] { String[].class });

		// figure out the arguments
		final List<String> arguments = new ArrayList<String>(Arrays.asList(args));
		arguments.add(0, "--warfile=" + me.getAbsolutePath());

		// override the usage screen
		final Field usage = launcher.getField("USAGE");
		usage.set(null, "JavaMelody Monitoring Collect Server " + "\n"
				+ "Usage: java -jar javamelody.war [--option=value] [--option=value]\n" + "\n"
				+ "Options:\n"
				+ "   --config                 = load configuration properties from here. Default is ./winstone.properties\n"
				+ "   --prefix                 = add this prefix to all URLs (eg http://localhost:8080/prefix/resource). Default is none\n"
				+ "   --commonLibFolder        = folder for additional jar files. Default is ./lib\n"
				+ "   \n"
				+ "   --logThrowingLineNo      = show the line no that logged the message (slow). Default is false\n"
				+ "   --logThrowingThread      = show the thread that logged the message. Default is false\n"
				+ "   --debug                  = set the level of debug msgs (1-9). Default is 5 (INFO level)\n"
				+ "\n"
				+ "   --httpPort               = set the http listening port. -1 to disable, Default is 8080\n"
				+ "   --httpListenAddress      = set the http listening address. Default is all interfaces\n"
				+ "   --httpDoHostnameLookups  = enable host name lookups on incoming http connections (true/false). Default is false\n"
				+ "   --httpKeepAliveTimeout   = how long idle HTTP keep-alive connections are kept around (in ms; default 5000)?\n"
				+ "   --httpsPort              = set the https listening port. -1 to disable, Default is disabled\n"
				+ "                              if neither --httpsCertificate nor --httpsKeyStore are specified,\n"
				+ "                              https is run with one-time self-signed certificate.\n"
				+ "   --httpsListenAddress     = set the https listening address. Default is all interfaces\n"
				+ "   --httpsDoHostnameLookups = enable host name lookups on incoming https connections (true/false). Default is false\n"
				+ "   --httpsKeepAliveTimeout   = how long idle HTTPS keep-alive connections are kept around (in ms; default 5000)?\n"
				+ "   --httpsKeyStore          = the location of the SSL KeyStore file.\n"
				+ "   --httpsKeyStorePassword  = the password for the SSL KeyStore file. Default is null\n"
				+ "   --httpsCertificate       = the location of the PEM-encoded SSL certificate file.\n"
				+ "                              (the one that starts with '-----BEGIN CERTIFICATE-----')\n"
				+ "                              must be used with --httpsPrivateKey.\n"
				+ "   --httpsPrivateKey        = the location of the PEM-encoded SSL private key.\n"
				+ "                              (the one that starts with '-----BEGIN RSA PRIVATE KEY-----')\n"
				+ "   --httpsKeyManagerType    = the SSL KeyManagerFactory type (eg SunX509, IbmX509). Default is SunX509\n"
				+ "   --spdy                   = Enable SPDY. See http://wiki.eclipse.org/Jetty/Feature/NPN\n"
				+ "   --ajp13Port              = set the ajp13 listening port. -1 to disable, Default is disabled\n"
				+ "   --ajp13ListenAddress     = set the ajp13 listening address. Default is all interfaces\n"
				+ "   --controlPort            = set the shutdown/control port. -1 to disable, Default disabled\n"
				+ "   \n"
				+ "   --handlerCountStartup    = set the no of worker threads to spawn at startup. Default is 5\n"
				+ "   --handlerCountMax        = set the max no of worker threads to allow. Default is 40\n"
				+ "   --handlerCountMaxIdle    = set the max no of idle worker threads to allow. Default is 5\n"
				+ "   \n"
				+ "   --sessionTimeout         = set the http session timeout value in minutes. Default to what webapp specifies, and then to 60 minutes\n"
				+ "   --mimeTypes=ARG          = define additional MIME type mappings. ARG would be EXT=MIMETYPE:EXT=MIMETYPE:...\n"
				+ "                              (e.g., xls=application/vnd.ms-excel:wmf=application/x-msmetafile)\n"
				+ "   --maxParamCount=N        = set the max number of parameters allowed in a form submission to protect\n"
				+ "                              against hash DoS attack (oCERT #2011-003). Default is 10000.\n"
				+ "   --usage / --help         = show this message\n" + "   \n"
				// For security of the collect server, see https://github.com/javamelody/javamelody/wiki/UserGuideAdvanced#5-security-with-a-collect-server
				// (-Djavamelody.authorized-users=user1:pwd1,user2:pwd2)
				//						+ "Security options:\n"
				//						+ "   --realmClassName               = Set the realm class to use for user authentication. Defaults to ArgumentsRealm class\n"
				//						+ "   \n"
				//						+ "   --argumentsRealm.passwd.<user> = Password for user <user>. Only valid for the ArgumentsRealm realm class\n"
				//						+ "   --argumentsRealm.roles.<user>  = Roles for user <user> (comma separated). Only valid for the ArgumentsRealm realm class\n"
				//						+ "   \n"
				//						+ "   --fileRealm.configFile         = File containing users/passwds/roles. Only valid for the FileRealm realm class\n"
				//						+ "   \n"
				+ "Access logging:\n"
				+ "   --accessLoggerClassName        = Set the access logger class to use for user authentication. Defaults to disabled\n"
				+ "   --simpleAccessLogger.format    = The log format to use. Supports combined/common/resin/custom (SimpleAccessLogger only)\n"
				+ "   --simpleAccessLogger.file      = The location pattern for the log file(SimpleAccessLogger only)");
		// run
		mainMethod.invoke(null, new Object[] { arguments.toArray(new String[arguments.size()]) });
	}

	// Figures out the location of <tt>javamelody.war</tt>.
	private static File whoAmI() throws IOException {
		// JNLP returns the URL where the jar was originally placed (like http://...)
		// not the local cached file. So we need a rather round about approach to get to
		// the local file name.
		// There is no portable way to find where the locally cached copy
		// of war/jar is; JDK 6 is too smart. (See JENKINS-2326.)
		try {
			return whoAmIFromJnlp();
		} catch (final Exception x) {
			getSystemErrorStream().println(
					"INFO: ZipFile.name trick did not work (" + x.toString() + "), using fallback");
		}
		final File myself = File.createTempFile("javamelody", ".jar");
		myself.deleteOnExit();
		final InputStream is = Main.class.getProtectionDomain().getCodeSource().getLocation()
				.openStream();
		try {
			final OutputStream os = new FileOutputStream(myself);
			try {
				copyStream(is, os);
			} finally {
				os.close();
			}
		} finally {
			is.close();
		}
		return myself;
	}

	/**
	 * @return System.out
	 */
	private static PrintStream getSystemOutputStream() {
		return System.out;
	}

	/**
	 * @return System.err
	 */
	private static PrintStream getSystemErrorStream() {
		return System.err;
	}

	private static File whoAmIFromJnlp() throws Exception {
		final URL classFile = Main.class.getClassLoader().getResource("Main.class");
		final JarFile jf = ((JarURLConnection) classFile.openConnection()).getJarFile();
		final Field f = ZipFile.class.getDeclaredField("name");
		f.setAccessible(true);
		return new File((String) f.get(jf));
	}

	private static void copyStream(InputStream in, OutputStream out) throws IOException {
		final byte[] buf = new byte[8192];
		int len;
		while ((len = in.read(buf)) > 0) {
			out.write(buf, 0, len);
		}
	}

	// Extract a resource from jar, mark it for deletion upon exit, and return its location.
	private static File extractFromJar(String resource, String fileName, String suffix)
			throws IOException {
		final URL res = Main.class.getResource(resource);

		// put this jar in a file system so that we can load jars from there
		final File tmp;
		try {
			tmp = File.createTempFile(fileName, suffix);
		} catch (final IOException e) {
			final String tmpdir = System.getProperty("java.io.tmpdir");
			throw new IllegalStateException(
					"JavaMelody has failed to create a temporary file in " + tmpdir, e);
		}
		final InputStream is = res.openStream();
		try {
			final OutputStream os = new FileOutputStream(tmp);
			try {
				copyStream(is, os);
			} finally {
				os.close();
			}
		} finally {
			is.close();
		}
		tmp.deleteOnExit();
		return tmp;
	}

	private static void deleteContents(File file) throws IOException {
		if (file.isDirectory()) {
			final File[] files = file.listFiles();
			// be defensive with null
			if (files != null) {
				for (final File file2 : files) {
					deleteContents(file2);
				}
			}
		}
		deleteFile(file);
	}

	private static boolean deleteFile(File file) {
		return file.delete();
	}

	//	/**
	//	 * {@link OutputStream} that writes to a log file.
	//	 *
	//	 * <p>
	//	 * Unlike the plain {@link FileOutputStream}, this implementation
	//	 * listens to SIGALRM and reopens the log file. This behavior is
	//	 * necessary for allowing log rotations to happen smoothly.
	//	 *
	//	 * <p>
	//	 * Because the reopen operation needs to happen atomically,
	//	 * write operations are synchronized.
	//	 *
	//	 * @author Kohsuke Kawaguchi
	//	 */
	//	@SuppressWarnings("all")
	//	private static final class LogFileOutputStream extends FilterOutputStream {
	//		/**
	//		 * /dev/null
	//		 */
	//		private static final OutputStream NULL = new OutputStream() {
	//			/** {@inheritDoc} */
	//			@Override
	//			public void write(int b) throws IOException {
	//				// noop
	//			}
	//
	//			/** {@inheritDoc} */
	//			@Override
	//			public void write(byte[] b, int off, int len) throws IOException {
	//				// noop
	//			}
	//		};
	//
	//		/**
	//		 * This is where we are writing.
	//		 */
	//		private final File file;
	//
	//		LogFileOutputStream(File file) throws FileNotFoundException {
	//			super(null);
	//			this.file = file;
	//			out = new FileOutputStream(file, true);
	//
	//			Signal.handle(new Signal("ALRM"), new SignalHandler() {
	//				public void handle(Signal signal) {
	//					try {
	//						reopen();
	//					} catch (final IOException e) {
	//						throw new Error(e); // failed to reopen
	//					}
	//				}
	//			});
	//		}
	//
	//		public synchronized void reopen() throws IOException {
	//			out.close();
	//			out = NULL; // in case reopen fails, initialize with NULL first
	//			out = new FileOutputStream(file, true);
	//		}
	//
	//		@Override
	//		public synchronized void write(byte[] b) throws IOException {
	//			out.write(b);
	//		}
	//
	//		@Override
	//		public synchronized void write(byte[] b, int off, int len) throws IOException {
	//			out.write(b, off, len);
	//		}
	//
	//		@Override
	//		public synchronized void flush() throws IOException {
	//			out.flush();
	//		}
	//
	//		@Override
	//		public synchronized void close() throws IOException {
	//			out.close();
	//		}
	//
	//		@Override
	//		public synchronized void write(int b) throws IOException {
	//			out.write(b);
	//		}
	//
	//		@Override
	//		public String toString() {
	//			return getClass().getName() + " -> " + file;
	//		}
	//	}
}
