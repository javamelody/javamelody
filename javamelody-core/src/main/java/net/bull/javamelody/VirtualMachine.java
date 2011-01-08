/*
 * Copyright 2008-2010 by Emeric Vernat
 *
 *     This file is part of Java Melody.
 *
 * Java Melody is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Java Melody is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Java Melody.  If not, see <http://www.gnu.org/licenses/>.
 */
package net.bull.javamelody;

import java.io.File;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;

/**
 * Classe d'attachement dynamique utilisée ici pour obtenir l'histogramme de la mémoire.
 * <br/>Cette classe nécessite tools.jar du jdk pour être exécutée (ok dans tomcat),
 * mais pas pour être compilée.
 * <br/>@see <a href="http://java.sun.com/javase/6/docs/jdk/api/attach/spec/com/sun/tools/attach/VirtualMachine.html#attach(java.lang.String)">VirtualMachine</a>
 * @author Emeric Vernat
 */
final class VirtualMachine {
	private static boolean enabled = isSupported();
	// singleton initialisé à la demande
	private static Object jvmVirtualMachine;

	private VirtualMachine() {
		super();
	}

	/**
	 * @return true si heapHisto supporté (jdk 1.6 de Sun ou de JRockit de BEA)
	 */
	static boolean isSupported() {
		// pour nodes hudson, on réévalue sans utiliser de constante
		final String javaVersion = System.getProperty("java.version");
		final String javaVendor = System.getProperty("java.vendor");
		return "1.6".compareTo(javaVersion) < 0
				&& (javaVendor.contains("Sun") || javaVendor.contains("Oracle")
						|| javaVendor.contains("Apple") || isJRockit());
	}

	/**
	 * @return true si JVM JRockit
	 */
	static boolean isJRockit() {
		// pour nodes hudson, on réévalue sans utiliser de constante
		return System.getProperty("java.vendor").contains("BEA");
	}

	/**
	 * @return false si non supporté ou si un attachement ou un histogramme a échoué,
	 * 		true si supporté et pas essayé ou si réussi
	 */
	static synchronized boolean isEnabled() { // NOPMD
		return enabled;
	}

	/**
	 * @return Singleton initialisé à la demande de l'instance de com.sun.tools.attach.VirtualMachine,
	 * 			null si enabled est false
	 * @throws Exception e
	 */
	static synchronized Object getJvmVirtualMachine() throws Exception { // NOPMD
		// si hotspot retourne une instance de sun.tools.attach.HotSpotVirtualMachine
		// cf http://www.java2s.com/Open-Source/Java-Document/6.0-JDK-Modules-sun/tools/sun/tools/attach/HotSpotVirtualMachine.java.htm
		// et sous windows : sun.tools.attach.WindowsVirtualMachine
		if (jvmVirtualMachine == null) {
			// on utilise la réflexion pour éviter de dépendre de tools.jar du jdk à la compilation
			final Class<?> virtualMachineClass = findVirtualMachineClass();
			final Method attachMethod = virtualMachineClass.getMethod("attach", String.class);
			final String pid = PID.getPID();
			try {
				jvmVirtualMachine = invoke(attachMethod, null, pid);
			} finally {
				enabled = jvmVirtualMachine != null;
			}
		}
		return jvmVirtualMachine;
	}

	private static Class<?> findVirtualMachineClass() throws ClassNotFoundException,
			MalformedURLException {
		// méthode inspirée de javax.tools.ToolProvider.Lazy.findClass
		final String virtualMachineClassName = "com.sun.tools.attach.VirtualMachine";
		try {
			return Class.forName(virtualMachineClassName);
		} catch (final ClassNotFoundException e) {
			// exception ignored, try looking else where
			File file = new File(System.getProperty("java.home"));
			if (file.getName().equalsIgnoreCase("jre")) {
				file = file.getParentFile();
			}
			final String[] defaultToolsLocation = { "lib", "tools.jar" };
			for (final String name : defaultToolsLocation) {
				file = new File(file, name);
			}
			final URL[] urls = { file.toURI().toURL() };
			final ClassLoader cl = URLClassLoader.newInstance(urls);
			return Class.forName(virtualMachineClassName, true, cl);
		}
	}

	/**
	 * Détachement du singleton.
	 * @throws Exception e
	 */
	static synchronized void detach() throws Exception { // NOPMD
		if (jvmVirtualMachine != null) {
			final Class<?> virtualMachineClass = jvmVirtualMachine.getClass();
			final Method detachMethod = virtualMachineClass.getMethod("detach");
			jvmVirtualMachine = invoke(detachMethod, jvmVirtualMachine);
			jvmVirtualMachine = null;
		}
	}

	/**
	 * @return flux contenant l'histogramme mémoire comme retourné par jmap -histo
	 * @throws Exception e
	 */
	static InputStream heapHisto() throws Exception { // NOPMD
		if (!isSupported()) {
			throw new IllegalStateException(I18N.getString("heap_histo_non_supporte"));
		}
		if (!isEnabled()) {
			throw new IllegalStateException(I18N.getString("heap_histo_non_actif"));
		}

		try {
			final Class<?> virtualMachineClass = getJvmVirtualMachine().getClass();
			final Method heapHistoMethod = virtualMachineClass.getMethod("heapHisto",
					Object[].class);
			return (InputStream) invoke(heapHistoMethod, getJvmVirtualMachine(),
					new Object[] { new Object[] { "-all" } });
		} catch (final ClassNotFoundException e) {
			// si on obtient ClassNotFoundException alors que heap histo est "supporté" (jdk 1.6 de Sun)
			// alors c'est que la jvm est un JRE et non un JDK (certainement avec tomcat) :
			// on le signale à l'administrateur car il peut simplement installer un JDK et changer JAVA_HOME,
			throw new IllegalStateException(I18N.getString("heap_histo_jre"), e);
		} catch (final Exception e) {
			// si on obtient com.sun.tools.attach.AttachNotSupportedException: no providers installed
			// alors c'est idem (javaws dans hudson nodes par exemple)
			if ("com.sun.tools.attach.AttachNotSupportedException".equals(e.getClass().getName())) {
				throw new IllegalStateException(I18N.getString("heap_histo_jre"), e);
			}
			throw e;
		}
	}

	/**
	 * @return l'histogramme mémoire
	 * @throws Exception e
	 */
	static HeapHistogram createHeapHistogram() throws Exception { // NOPMD
		final InputStream input = heapHisto();
		try {
			return new HeapHistogram(input, isJRockit());
		} finally {
			input.close();
		}
	}

	private static Object invoke(Method method, Object object, Object... args) throws Exception { // NOPMD
		try {
			return method.invoke(object, args);
		} catch (final InvocationTargetException e) {
			if (e.getCause() instanceof Exception) {
				throw (Exception) e.getCause();
			} else if (e.getCause() instanceof Error) {
				throw (Error) e.getCause();
			} else {
				throw new Exception(e.getCause()); // NOPMD
			}
		}
	}

	// Note : on pourrait aussi charger dynamiquement un agent avec jvmVirtualMachine.loadAgent
	// (en générant un jar temporaire avec ZipFile à partir de
	// getClass().getResourceAsStream(getClass().getName() + ".class"), d'un manifest contenant Agent-Class)
	// pour obtenir la taille sans ses références d'un objet (Instrumentation.getObjectSize)
	// ou pour ajouter de la programmation par aspect à la volée (datasource jdbc, façades...)
	// (addClassTransformer(new org.aspectj.weaver.loadtime.ClassPreProcessorAgentAdapter())
	// ou loadAgent("aspectjweaver.jar") par exemple).
	// Voir http://java.sun.com/javase/6/docs/api/java/lang/instrument/package-summary.html?is-external=true

	//	private static final String MONITORING_INSTRUMENTATION_KEY = "javamelody.instrumentation";
	//	private static Instrumentation instrumentation;
	//
	//	public void loadAgent(String jarFile) throws AgentLoadException, AgentInitializationException,
	//			IOException {
	//		try {
	//			jvmVirtualMachine.loadAgent(jarFile);
	//		} finally {
	//			instrumentation = (Instrumentation) System.getProperties().get(
	//					MONITORING_INSTRUMENTATION_KEY);
	//			System.getProperties().remove(MONITORING_INSTRUMENTATION_KEY);
	//		}
	//	}
	//
	//	public static void agentmain(@SuppressWarnings("unused") String agentArgs, Instrumentation inst) {
	//		System.getProperties().put(MONITORING_INSTRUMENTATION_KEY, inst);
	//	}
	//
	//	public long getObjectSize(Object objectToSize) {
	//		return instrumentation.getObjectSize(objectToSize);
	//	}
	//
	//	public void addClassTransformer(ClassFileTransformer transformer, boolean canRetransform) {
	//		instrumentation.addTransformer(transformer, canRetransform);
	//	}
}
