/*
 * Copyright 2008-2019 by Emeric Vernat
 *
 *     This file is part of Java Melody.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.bull.javamelody.internal.model;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLClassLoader;

import javax.management.JMException;
import javax.management.ObjectName;

import net.bull.javamelody.internal.common.I18N;

/**
 * Classe d'attachement dynamique utilisée ici pour obtenir l'histogramme de la mémoire.
 * <br/>Cette classe nécessite tools.jar du jdk pour être exécutée (ok dans tomcat),
 * mais pas pour être compilée.
 * <br/>@see <a href="http://java.sun.com/javase/6/docs/jdk/api/attach/spec/com/sun/tools/attach/VirtualMachine.html#attach(java.lang.String)">VirtualMachine</a>
 * @author Emeric Vernat
 */
public final class VirtualMachine {
	private static boolean enabled = isSupported();
	// singleton initialisé à la demande
	private static Object jvmVirtualMachine;

	private VirtualMachine() {
		super();
	}

	/**
	 * @return true si heapHisto supporté.
	 */
	public static boolean isSupported() {
		// pour nodes Jenkins, on réévalue sans utiliser de constante
		final String javaVendor = System.getProperty("java.vendor");
		return javaVendor.contains("Sun") || javaVendor.contains("Oracle")
				|| javaVendor.contains("Apple") || isJRockit();
	}

	/**
	 * @return true si JVM JRockit
	 */
	public static boolean isJRockit() {
		// pour nodes Jenkins, on réévalue sans utiliser de constante
		return System.getProperty("java.vendor").contains("BEA");
	}

	/**
	 * @return false si non supporté ou si un attachement ou un histogramme a échoué,
	 * 		true si supporté et pas essayé ou si réussi
	 */
	public static synchronized boolean isEnabled() { // NOPMD
		return enabled;
	}

	/**
	 * @return Singleton initialisé à la demande de l'instance de com.sun.tools.attach.VirtualMachine,
	 * 			null si enabled est false
	 * @throws Exception e
	 */
	public static synchronized Object getJvmVirtualMachine() throws Exception { // NOPMD
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

	private static Class<?> findVirtualMachineClass() throws Exception { // NOPMD
		// méthode inspirée de javax.tools.ToolProvider.Lazy.findClass
		// http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/6-b27/javax/tools/ToolProvider.java#ToolProvider.Lazy.findClass%28%29
		final String virtualMachineClassName = "com.sun.tools.attach.VirtualMachine";
		try {
			// try loading class directly, in case tools.jar is in the classpath
			return Class.forName(virtualMachineClassName);
		} catch (final ClassNotFoundException e) {
			// exception ignored, try looking in the default tools location (lib/tools.jar)
			File file = new File(System.getProperty("java.home"));
			if ("jre".equalsIgnoreCase(file.getName())) {
				file = file.getParentFile();
			}
			final String[] defaultToolsLocation = { "lib", "tools.jar" };
			for (final String name : defaultToolsLocation) {
				file = new File(file, name);
			}
			// if tools.jar not found, no point in trying a URLClassLoader
			// so rethrow the original exception.
			if (!file.exists()) {
				throw e;
			}

			final URL url = file.toURI().toURL();
			final ClassLoader cl;
			//			if (ClassLoader.getSystemClassLoader() instanceof URLClassLoader) {
			//				// The attachment API relies on JNI, so if we have other code in the JVM that tries to use the attach API
			//				// (like the monitoring of another webapp), it'll cause a failure (issue 398):
			//				// "UnsatisfiedLinkError: Native Library C:\Program Files\Java\jdk1.6.0_35\jre\bin\attach.dll already loaded in another classloader
			//				// [...] com.sun.tools.attach.AttachNotSupportedException: no providers installed"
			//				// So we try to load tools.jar into the system classloader, so that later attempts to load tools.jar will see it.
			//				cl = ClassLoader.getSystemClassLoader();
			//				// The URLClassLoader.addURL method is protected
			//				final Method addURL = URLClassLoader.class.getDeclaredMethod("addURL", URL.class);
			//				addURL.setAccessible(true);
			//				addURL.invoke(cl, url);
			//			} else {
			final URL[] urls = { url };
			cl = URLClassLoader.newInstance(urls);
			//			}
			return Class.forName(virtualMachineClassName, true, cl);
		}
	}

	/**
	 * Détachement du singleton.
	 * @throws Exception e
	 */
	public static synchronized void detach() throws Exception { // NOPMD
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
	public static InputStream heapHisto() throws Exception { // NOPMD
		if (!isSupported()) {
			throw new IllegalStateException(I18N.getString("heap_histo_non_supporte"));
		}
		if (!isEnabled()) {
			throw new IllegalStateException(I18N.getString("heap_histo_non_actif"));
		}

		try {
			final ObjectName objectName = new ObjectName(
					"com.sun.management:type=DiagnosticCommand");
			final String gcClassHistogram = (String) MBeansAccessor.invoke(objectName,
					"gcClassHistogram", new Object[] { null }, new Class[] { String[].class });
			return new ByteArrayInputStream(gcClassHistogram.getBytes("UTF-8"));
		} catch (final JMException e1) {
			// MBean "DiagnosticCommand" not found (with JDK 7 for example),
			// continue with VM attach method
			try {
				final Class<?> virtualMachineClass = getJvmVirtualMachine().getClass();
				final Method heapHistoMethod = virtualMachineClass.getMethod("heapHisto",
						Object[].class);
				return (InputStream) invoke(heapHistoMethod, getJvmVirtualMachine(),
						new Object[] { new Object[] { "-all" } });
			} catch (final ClassNotFoundException e) {
				// si on obtient ClassNotFoundException alors que heap histo est "supporté"
				// alors c'est que la jvm est un JRE et non un JDK (certainement avec tomcat) :
				// on le signale à l'administrateur car il peut simplement installer un JDK et changer JAVA_HOME,
				throw new IllegalStateException(I18N.getString("heap_histo_jre"), e);
			} catch (final Exception e) {
				if ("Can not attach to current VM".equals(e.getMessage())) {
					throw new IllegalStateException(I18N.getString("allowAttachSelf"), e);
				}
				// si on obtient com.sun.tools.attach.AttachNotSupportedException: no providers installed
				// alors c'est idem (javaws dans Jenkins nodes par exemple)
				if ("com.sun.tools.attach.AttachNotSupportedException"
						.equals(e.getClass().getName())) {
					throw new IllegalStateException(I18N.getString("heap_histo_jre"), e);
				}
				throw e;
			}
		}
	}

	/**
	 * @return l'histogramme mémoire
	 * @throws Exception e
	 */
	public static HeapHistogram createHeapHistogram() throws Exception { // NOPMD
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
