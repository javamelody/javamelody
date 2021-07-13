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
package net.bull.javamelody.internal.model; // NOPMD

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TreeMap;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import javax.servlet.ServletContext;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import net.bull.javamelody.Parameter;
import net.bull.javamelody.internal.common.InputOutput;
import net.bull.javamelody.internal.common.LOG;
import net.bull.javamelody.internal.common.Parameters;

/**
 * Lecture d'artifacts Maven.
 * @author Emeric Vernat
 */
public final class MavenArtifact implements Serializable {
	private static final long serialVersionUID = 1L;

	private static final String MAVEN_CENTRAL = "https://repo1.maven.org/maven2";

	private static final File LOCAL_REPO = new File(
			System.getProperty("user.home") + "/.m2/repository");

	private static final String TOMCAT_ARCHIVES = "https://archive.apache.org/dist/tomcat/";

	private static Map<String, String> sourceFilePathsByJarFileNames;

	private static String webappVersion;

	private String name;
	private String url;
	private String groupId;
	private String artifactId;
	private String version;
	private MavenArtifact parent;
	private final Map<String, String> licenseUrlsByName = new LinkedHashMap<>();
	private Map<String, String> properties;
	private final List<MavenArtifact> dependencies = new ArrayList<>();
	private final List<MavenArtifact> managedDependencies = new ArrayList<>();
	private boolean updated;

	private MavenArtifact() {
		super();
	}

	private static MavenArtifact parseDependency(URL jarFileLocation) throws IOException {
		final byte[] pomXml = readMavenFileFromJarFile(jarFileLocation, "pom.xml");
		if (pomXml != null) {
			final MavenArtifact dependency = new MavenArtifact();
			dependency.parsePomXml(new ByteArrayInputStream(pomXml));
			return dependency;
		}
		return null;
	}

	private void parsePomXml(InputStream pomXml) throws IOException {
		final DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
		try {
			final DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
			final Document doc = dBuilder.parse(pomXml);
			final Node projectNode = doc.getElementsByTagName("project").item(0);
			final NodeList childNodes = projectNode.getChildNodes();
			properties = new HashMap<>();
			for (int i = 0; i < childNodes.getLength(); i++) {
				final Node node = childNodes.item(i);
				parseNode(node);
			}
			properties.put("project.groupId", groupId);
			properties.put("pom.groupId", groupId);
			properties.put("groupId", groupId);
			properties.put("project.version", version);
			properties.put("pom.version", version);
			properties.put("version", version);
			for (final MavenArtifact dependency : dependencies) {
				dependency.groupId = replaceProperty(dependency.groupId, properties);
				dependency.version = replaceProperty(dependency.version, properties);
			}
			for (final MavenArtifact dependency : managedDependencies) {
				dependency.groupId = replaceProperty(dependency.groupId, properties);
				dependency.version = replaceProperty(dependency.version, properties);
			}
			properties = null;
		} catch (final ParserConfigurationException | SAXException e) {
			throw new IOException(e.getMessage(), e);
		}
		updated = true;
	}

	// CHECKSTYLE:OFF
	private void parseNode(Node node) {
		// CHECKSTYLE:ON
		final String nodeName = node.getNodeName();
		if ("name".equals(nodeName)) {
			this.name = node.getTextContent();
		} else if ("description".equals(nodeName) && this.name == null) {
			this.name = node.getTextContent();
		} else if ("url".equals(nodeName)) {
			this.url = node.getTextContent();
		} else if ("groupId".equals(nodeName) && !node.getTextContent().startsWith("${")) {
			this.groupId = node.getTextContent();
		} else if ("artifactId".equals(nodeName)) {
			this.artifactId = node.getTextContent();
		} else if ("version".equals(nodeName) && !node.getTextContent().startsWith("${")) {
			this.version = node.getTextContent();
		} else if ("parent".equals(nodeName)) {
			parseParentNode(node);
		} else if ("properties".equals(nodeName)) {
			properties.putAll(parsePropertiesNode(node));
		} else if ("licenses".equals(nodeName)) {
			parseLicensesNode(node);
		} else if ("dependencies".equals(nodeName)) {
			this.dependencies.addAll(parseDependenciesNode(node));
		} else if ("dependencyManagement".equals(nodeName)) {
			final NodeList childNodes = node.getChildNodes();
			for (int i = 0; i < childNodes.getLength(); i++) {
				final Node childNode = childNodes.item(i);
				if ("dependencies".equals(childNode.getNodeName())) {
					this.managedDependencies.addAll(parseDependenciesNode(childNode));
				}
			}
		}
	}

	private void parseParentNode(Node node) {
		final NodeList parentNodes = node.getChildNodes();
		String parentGroupId = null;
		String parentArtifactId = null;
		String parentVersion = null;
		for (int k = 0; k < parentNodes.getLength(); k++) {
			final Node childParentNode = parentNodes.item(k);
			final String nodeName = childParentNode.getNodeName();
			if ("groupId".equals(nodeName)) {
				parentGroupId = childParentNode.getTextContent();
			} else if ("artifactId".equals(nodeName)) {
				parentArtifactId = childParentNode.getTextContent();
			} else if ("version".equals(nodeName)) {
				parentVersion = childParentNode.getTextContent();
			}
		}
		if (this.groupId == null) {
			this.groupId = parentGroupId;
		}
		if (this.version == null) {
			this.version = parentVersion;
		}
		this.parent = new MavenArtifact();
		this.parent.groupId = parentGroupId;
		this.parent.artifactId = parentArtifactId;
		this.parent.version = parentVersion;
	}

	private Map<String, String> parsePropertiesNode(Node propertiesNode) {
		final Map<String, String> props = new HashMap<>();
		final NodeList propertiesNodes = propertiesNode.getChildNodes();
		for (int j = 0; j < propertiesNodes.getLength(); j++) {
			final Node propertyNode = propertiesNodes.item(j);
			final String nodeName = propertyNode.getNodeName();
			if (nodeName != null) {
				props.put(nodeName, propertyNode.getTextContent());
			}
		}
		return props;
	}

	private void parseLicensesNode(Node licensesNode) {
		final NodeList licenseNodes = licensesNode.getChildNodes();
		for (int j = 0; j < licenseNodes.getLength(); j++) {
			final Node licenseNode = licenseNodes.item(j);
			if ("license".equals(licenseNode.getNodeName())) {
				String licenseName = null;
				String licenseUrl = null;
				final NodeList childLicenseNodes = licenseNode.getChildNodes();
				for (int k = 0; k < childLicenseNodes.getLength(); k++) {
					final Node childLicenseNode = childLicenseNodes.item(k);
					final String nodeName = childLicenseNode.getNodeName();
					if ("name".equals(nodeName)) {
						licenseName = childLicenseNode.getTextContent();
					} else if ("url".equals(nodeName)) {
						licenseUrl = childLicenseNode.getTextContent();
					}
				}
				if (licenseName != null) {
					licenseUrlsByName.put(licenseName, licenseUrl);
				} else if (licenseUrl != null) {
					if (licenseUrl.startsWith("http")) {
						licenseUrlsByName.put("LICENSE", licenseUrl);
					} else {
						licenseUrlsByName.put(licenseUrl, licenseUrl);
					}
				}
			}
		}
	}

	// CHECKSTYLE:OFF
	private List<MavenArtifact> parseDependenciesNode(Node dependenciesNode) {
		// CHECKSTYLE:ON
		final List<MavenArtifact> deps = new ArrayList<>();
		final NodeList dependencyNodes = dependenciesNode.getChildNodes();
		for (int j = 0; j < dependencyNodes.getLength(); j++) {
			final Node dependencyNode = dependencyNodes.item(j);
			if ("dependency".equals(dependencyNode.getNodeName())) {
				final NodeList childDependencyNodes = dependencyNode.getChildNodes();
				final MavenArtifact dependency = new MavenArtifact();
				String scope = null;
				String optional = null;
				for (int k = 0; k < childDependencyNodes.getLength(); k++) {
					final Node childDependencyNode = childDependencyNodes.item(k);
					final String nodeName = childDependencyNode.getNodeName();
					if ("groupId".equals(nodeName)) {
						dependency.groupId = childDependencyNode.getTextContent();
					} else if ("artifactId".equals(nodeName)) {
						dependency.artifactId = childDependencyNode.getTextContent();
					} else if ("version".equals(nodeName)) {
						dependency.version = childDependencyNode.getTextContent();
					} else if ("scope".equals(nodeName)) {
						scope = childDependencyNode.getTextContent();
					} else if ("optional".equals(nodeName)) {
						optional = childDependencyNode.getTextContent();
					}
				}
				if ((scope == null || "compile".equals(scope)) && !"true".equals(optional)) {
					deps.add(dependency);
				}
			}
		}
		return deps;
	}

	private void update() throws IOException {
		if (!updated && version != null) {
			updated = true;
			final String filePath = getPath(".pom");
			final File pomXml = getMavenArtifact(filePath);
			if (pomXml != null) {
				try (InputStream input = new FileInputStream(pomXml)) {
					parsePomXml(input);
				}
			}
		}
	}

	public String getGroupId() {
		return groupId;
	}

	public String getArtifactId() {
		return artifactId;
	}

	public String getVersion() {
		return version;
	}

	public String getName() throws IOException {
		if (name == null && parent != null) {
			parent.update();
			return parent.getName();
		}
		return name;
	}

	public String getUrl() throws IOException {
		if (url == null && parent != null) {
			parent.update();
			return parent.getUrl();
		}
		return url;
	}

	public Map<String, String> getLicenseUrlsByName() throws IOException {
		if (licenseUrlsByName.isEmpty() && parent != null) {
			parent.update();
			return parent.getLicenseUrlsByName();
		}
		return licenseUrlsByName;
	}

	// not needed:
	//	MavenArtifact getParent() {
	//		return parent;
	//	}
	//
	//	List<MavenArtifact> getDependencies() {
	//		return dependencies;
	//	}
	//
	//	List<MavenArtifact> getManagedDependencies() {
	//		return managedDependencies;
	//	}

	private List<MavenArtifact> getAllManagedDependencies() throws IOException {
		update();
		final List<MavenArtifact> allManagedDependencies = new ArrayList<>(managedDependencies);
		if (parent != null) {
			allManagedDependencies.addAll(parent.getAllManagedDependencies());
		}
		return allManagedDependencies;
	}

	List<MavenArtifact> getAllDependencies() throws IOException {
		return getAllDependencies(1);
	}

	private List<MavenArtifact> getAllDependencies(int level) throws IOException {
		if (level > 10) {
			// limit recursivity against cycle (for example dom4j 1.5.2 <-> jaxen 1.1-beta-4)
			return Collections.emptyList();
		}
		// update dependencies if needed
		update();
		final List<MavenArtifact> transitiveDependencies = new ArrayList<>();
		final List<MavenArtifact> allManagedDependencies = getAllManagedDependencies();
		for (final MavenArtifact dependency : dependencies) {
			if (dependency.version == null) {
				for (final MavenArtifact managedDependency : allManagedDependencies) {
					if (dependency.isSame(managedDependency)) {
						dependency.version = managedDependency.version;
						break;
					}
				}
			}
			transitiveDependencies.addAll(dependency.getAllDependencies(level + 1));
		}
		if (parent != null) {
			transitiveDependencies.addAll(parent.getAllDependencies(level + 1));
		}

		final List<MavenArtifact> allDependencies = new ArrayList<>(dependencies);
		for (final MavenArtifact transitiveDependency : transitiveDependencies) {
			if (!transitiveDependency.isContained(allDependencies)) {
				allDependencies.add(transitiveDependency);
			}
		}
		return allDependencies;
	}

	private boolean isContained(List<MavenArtifact> artifacts) {
		for (final MavenArtifact artifact : artifacts) {
			if (isSame(artifact)) {
				return true;
			}
		}
		return false;
	}

	private boolean isSame(MavenArtifact artifact) {
		return groupId.equals(artifact.groupId) && artifactId.equals(artifact.artifactId);
	}

	private String getPath(String extension) {
		return groupId.replace('.', '/') + '/' + artifactId + '/' + version + '/' + artifactId + '-'
				+ version + extension;
	}

	public static Map<String, MavenArtifact> getWebappDependencies() throws IOException {
		// list dependencies in WEB-INF/lib
		// and read names, urls, licences in META-INF/maven/.../pom.xml from jar files when available
		final Map<String, MavenArtifact> webappDependencies = getWebappDependenciesFromWebInfLib();

		// when pom.xml not available in some jar files,
		// list all dependencies in webapp's pom.xml if it exists or in the other dependencies' pom.xml,
		// including transitive dependencies
		final List<MavenArtifact> allDependencies = new ArrayList<>(
				getWebappDependenciesFromPomXml());
		for (final MavenArtifact dependency : webappDependencies.values()) {
			if (dependency != null && !dependency.isContained(allDependencies)) {
				allDependencies.add(dependency);
				for (final MavenArtifact transitiveDependency : dependency.getAllDependencies()) {
					if (!transitiveDependency.isContained(allDependencies)) {
						allDependencies.add(transitiveDependency);
					}
				}
			}
		}
		// in order to complete names, urls, licences from all dependencies and parents
		for (final Map.Entry<String, MavenArtifact> entry : webappDependencies.entrySet()) {
			if (entry.getValue() == null) {
				final String jarFileName = entry.getKey();
				for (final MavenArtifact dependency : allDependencies) {
					if (jarFileName.startsWith(
							dependency.getArtifactId() + '-' + dependency.getVersion())) {
						entry.setValue(dependency);
						break;
					}
				}
			}
		}
		return webappDependencies;
	}

	private static List<MavenArtifact> getWebappDependenciesFromPomXml() throws IOException {
		final InputStream webappPomXmlAsStream = getWebappPomXmlAsStream();
		if (webappPomXmlAsStream != null) {
			try {
				final MavenArtifact webappArtifact = new MavenArtifact();
				webappArtifact.parsePomXml(webappPomXmlAsStream);
				return webappArtifact.getAllDependencies();
			} finally {
				webappPomXmlAsStream.close();
			}
		}
		return Collections.emptyList();
	}

	private static Map<String, MavenArtifact> getWebappDependenciesFromWebInfLib()
			throws IOException {
		final ServletContext servletContext = Parameters.getServletContext();
		final String directory = "/WEB-INF/lib/";

		final Set<String> dependencies = servletContext.getResourcePaths(directory);
		// If needed, catch Exception again because Tomcat 8 can throw
		// "IllegalStateException: The resources may not be accessed if they are not currently started"
		// for some ServletContext states (issue 415)
		if (dependencies == null || dependencies.isEmpty()) {
			return Collections.emptyMap();
		}
		final Map<String, MavenArtifact> result = new TreeMap<>();
		for (final String dependency : dependencies) {
			if (dependency.endsWith(".jar") || dependency.endsWith(".JAR")) {
				final String fileName = dependency.substring(directory.length());
				final URL jarFileLocation = servletContext.getResource(dependency);
				if (jarFileLocation != null) {
					result.put(fileName, parseDependency(jarFileLocation));
				} else {
					result.put(fileName, null);
				}
			}
		}
		return result;
	}

	public static InputStream getWebappPomXmlAsStream() {
		return getWebappPomFile("pom.xml");
	}

	public static synchronized String getWebappVersion() {
		if (webappVersion == null) {
			webappVersion = Parameter.APPLICATION_VERSION.getValue();
			if (webappVersion == null) {
				final InputStream input = getWebappPomFile("pom.properties");
				if (input != null) {
					try {
						try {
							final Properties properties = new Properties();
							properties.load(input);
							webappVersion = properties.getProperty("version");
						} finally {
							input.close();
						}
					} catch (final IOException e) {
						LOG.debug(e.toString(), e);
					}
				}
				if (webappVersion == null) {
					// remember that the webapp version can't be found
					webappVersion = "";
				}
			}
		}
		if (webappVersion.isEmpty()) {
			return null;
		}
		return webappVersion;
	}

	private static InputStream getWebappPomFile(String pomFilename) {
		final Set<?> mavenDir = Parameters.getServletContext().getResourcePaths("/META-INF/maven/");
		if (mavenDir == null || mavenDir.isEmpty()) {
			return null;
		}
		final Set<?> groupDir = Parameters.getServletContext()
				.getResourcePaths((String) mavenDir.iterator().next());
		if (groupDir == null || groupDir.isEmpty()) {
			return null;
		}
		final InputStream pomXml = Parameters.getServletContext()
				.getResourceAsStream(groupDir.iterator().next() + pomFilename);
		if (pomXml == null) {
			return null;
		}
		return new BufferedInputStream(pomXml);
	}

	public static File getSourceJarFile(URL classesJarFileUrl) throws IOException {
		final String file = classesJarFileUrl.getFile();
		if (file.endsWith(".jar")) {
			final File sources = new File(file.replace(".jar", "-sources.jar"));
			if (sources.exists()) {
				return sources;
			}
		}
		final byte[] pomProperties = readMavenFileFromJarFile(classesJarFileUrl, "pom.properties");
		if (pomProperties == null) {
			final Map<String, String> sourceFilePaths = getSourceFilePathsByJarFileNames();
			String jarFileName = file;
			if (jarFileName.endsWith("!/")) {
				// remove "!/" at the end, for spring-boot launched with "java -jar"
				jarFileName = jarFileName.substring(0, jarFileName.length() - "!/".length());
			}
			jarFileName = jarFileName.substring(jarFileName.lastIndexOf('/') + 1);
			final String sourceFilePath = sourceFilePaths.get(jarFileName);
			if (sourceFilePath != null) {
				return getMavenArtifact(sourceFilePath);
			}
			return null;
		}
		final Properties properties = new Properties();
		properties.load(new ByteArrayInputStream(pomProperties));
		final MavenArtifact mavenArtifact = new MavenArtifact();
		mavenArtifact.groupId = properties.getProperty("groupId");
		mavenArtifact.artifactId = properties.getProperty("artifactId");
		mavenArtifact.version = properties.getProperty("version");
		final String filePath = mavenArtifact.getPath("-sources.jar");
		return getMavenArtifact(filePath);
	}

	private static synchronized Map<String, String> getSourceFilePathsByJarFileNames()
			throws IOException {
		if (sourceFilePathsByJarFileNames == null) {
			final Map<String, MavenArtifact> webappDependencies = getWebappDependencies();
			final Map<String, String> sourceFilePaths = new HashMap<>();
			for (final Map.Entry<String, MavenArtifact> entry : webappDependencies.entrySet()) {
				final String jarFileName = entry.getKey();
				final MavenArtifact dependency = entry.getValue();
				if (dependency != null) {
					final String filePath = dependency.getPath("-sources.jar");
					sourceFilePaths.put(jarFileName, filePath);
				}
			}
			sourceFilePathsByJarFileNames = sourceFilePaths;
		}
		return sourceFilePathsByJarFileNames;
	}

	private static File getMavenArtifact(String filePath) {
		if (filePath.contains("${")) {
			// si le chemin contient des variables non résolues telles que ${project.version},
			// ce n'est pas la peine de chercher
			return null;
		}
		final File storageDirectory = Parameters
				.getStorageDirectory(Parameters.getCurrentApplication());
		final String subDirectory;
		if (filePath.endsWith(".pom")) {
			subDirectory = "poms";
		} else {
			subDirectory = "sources";
		}
		final File file = new File(storageDirectory,
				subDirectory + '/' + filePath.substring(filePath.lastIndexOf('/') + 1));
		if (!file.exists() || file.length() == 0) {
			for (final String mavenRepository : getMavenRepositories()) {
				final String url = mavenRepository + '/' + filePath;
				if (!url.startsWith("http")) {
					if (!new File(url).exists()) {
						continue;
					}
					return new File(url);
				}
				mkdirs(file.getParentFile());
				try (OutputStream output = new FileOutputStream(file)) {
					final LabradorRetriever labradorRetriever = new LabradorRetriever(new URL(url));
					labradorRetriever.downloadTo(output);
					// si trouvé, on arrête
					break;
				} catch (final IOException e) {
					InputOutput.deleteFile(file);
					// si non trouvé, on continue avec le repo suivant s'il y en a un
				}
			}
		}
		if (file.exists()) {
			return file;
		}
		return null;
	}

	public static File getTomcatSrcZipFile() {
		final String serverInfo = Parameters.getServletContext().getServerInfo();
		if (!serverInfo.matches("Apache Tomcat/\\d+\\.\\d+\\.\\d+")) {
			// si pas Tomcat ou si Tomcat version x.0.0.My, tant pis
			return null;
		}
		final String version = serverInfo.substring(serverInfo.lastIndexOf('/') + 1);
		final String fileName = "apache-tomcat-" + version + "-src.zip";
		final File storageDirectory = Parameters
				.getStorageDirectory(Parameters.getCurrentApplication());
		final String subDirectory = "sources";
		final File file = new File(storageDirectory, subDirectory + '/' + fileName);
		if (!file.exists() || file.length() == 0) {
			final String majorVersion = version.substring(0, version.indexOf('.'));
			final String url = TOMCAT_ARCHIVES + "tomcat-" + majorVersion + "/v" + version + "/src/"
					+ fileName;
			mkdirs(file.getParentFile());
			try (OutputStream output = new FileOutputStream(file)) {
				final LabradorRetriever labradorRetriever = new LabradorRetriever(new URL(url));
				labradorRetriever.downloadTo(output);
			} catch (final IOException e) {
				InputOutput.deleteFile(file);
			}
		}
		if (file.exists()) {
			return file;
		}
		return null;
	}

	private static void mkdirs(File directory) {
		if (!directory.exists() && !directory.mkdirs()) {
			throw new IllegalStateException("Can't create directory " + directory.getPath());
		}
	}

	private static byte[] readMavenFileFromJarFile(URL jarFileLocation, String pomFileName)
			throws IOException {
		try (ZipInputStream zipInputStream = new ZipInputStream(
				new BufferedInputStream(jarFileLocation.openStream(), 4096))) {
			ZipEntry entry = zipInputStream.getNextEntry();
			while (entry != null) {
				if (entry.getName().startsWith("META-INF/maven/")
						&& entry.getName().endsWith("/" + pomFileName)) {
					return InputOutput.pumpToByteArray(zipInputStream);
				}
				zipInputStream.closeEntry();
				entry = zipInputStream.getNextEntry();
			}
		}
		return null;
	}

	private static List<String> getMavenRepositories() {
		final String parameter = Parameter.MAVEN_REPOSITORIES.getValue();
		if (parameter != null) {
			final List<String> result = new ArrayList<>();
			for (final String repo : parameter.split(",")) {
				result.add(repo.trim());
			}
			return result;
		}
		return Arrays.asList(LOCAL_REPO.getPath(), MAVEN_CENTRAL);
	}

	private static String replaceProperty(String value, Map<String, String> properties) {
		if (value != null && value.startsWith("${") && value.endsWith("}")) {
			final String propertyName = value.substring("${".length(),
					value.length() - "}".length());
			final String propertyValue = properties.get(propertyName);
			if (propertyValue != null) {
				return propertyValue;
			}
		}
		return value;
	}

	@Override
	public String toString() {
		return getClass().getSimpleName() + '[' + groupId + ':' + artifactId + ':' + version + ']';
	}
}
