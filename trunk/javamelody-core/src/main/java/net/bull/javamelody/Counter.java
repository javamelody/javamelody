/*
 * Copyright 2008-2009 by Emeric Vernat, Bull
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

import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.regex.Pattern;

/**
 * Données statistiques des requêtes pour un compteur nommé comme http ou sql.
 * Ces données sont accumulées au fil du temps selon les requêtes dans l'application.
 * Elles correspondent soit aux statistiques courantes depuis une date initiale,
 * soit à une période donnée pour un jour, une semaine, un mois ou une année.
 *
 * Toutes les méthodes sur une instance de cette classe sont conçues pour être thread-safe,
 * c'est-à-dire qu'elles gère un état qui est non modifiable
 * ou alors synchronisé pour être accessible et modifiable depuis plusieurs threads.
 * Les instances sont sérialisables pour pouvoir être persistées sur disque
 * et transmises au serveur de collecte.
 * @author Emeric Vernat
 */
class Counter implements Cloneable, Serializable {
	/**
	 * Nom du counter des erreurs systèmes http.
	 */
	static final String ERROR_COUNTER_NAME = "error";
	/**
	 * Nom du counter des logs d'erreurs systèmes.
	 */
	static final String LOG_COUNTER_NAME = "log";
	/**
	 * Nombre max d'erreurs conservées par le counter (si counter d'erreurs http ou de log d'erreurs).
	 */
	static final int MAX_ERRORS_COUNT = 100;
	/**
	 * Nombre max de requêtes conservées par counter.
	 */
	private static final int MAX_REQUESTS_COUNT = 20000;
	private static final long serialVersionUID = 6759729262180992976L;
	private String application;
	private boolean displayed = true;
	private final String name;
	private final boolean errorCounter;
	private final String storageName;
	private final String iconName;
	// on conserve childCounterName et pas childCounter pour assurer la synchronisation/clone et la sérialisation
	private final String childCounterName;
	@SuppressWarnings("all")
	private final ConcurrentMap<String, CounterRequest> requests = new ConcurrentHashMap<String, CounterRequest>();
	@SuppressWarnings("all")
	private final ConcurrentMap<Long, CounterRequestContext> rootCurrentContextsByThreadId = new ConcurrentHashMap<Long, CounterRequestContext>();
	//CHECKSTYLE:OFF
	private final LinkedList<CounterError> errors; // NOPMD
	//CHECKSTYLE:ON
	private Date startDate = new Date();
	private int maxRequestsCount = MAX_REQUESTS_COUNT;
	// Pour les contextes, on utilise un ThreadLocal et pas un InheritableThreadLocal
	// puisque si on crée des threads alors la requête parente peut se terminer avant les threads
	// et le contexte serait incomplet.
	private final transient ThreadLocal<CounterRequestContext> contextThreadLocal;
	private transient Pattern requestTransformPattern;

	/**
	 * Comparateur pour ordonner les requêtes par sommes des durées.
	 */
	static final class CounterRequestComparator implements Comparator<CounterRequest>, Serializable {
		private static final long serialVersionUID = 1L;

		/** {@inheritDoc} */
		public int compare(CounterRequest request1, CounterRequest request2) {
			if (request1.getDurationsSum() > request2.getDurationsSum()) {
				return 1;
			} else if (request1.getDurationsSum() < request2.getDurationsSum()) {
				return -1;
			} else {
				return 0;
			}
		}
	}

	/**
	 * Comparateur pour ordonner les requêtes par nombre d'exécutions.
	 */
	static final class CounterRequestByHitsComparator implements Comparator<CounterRequest>,
			Serializable {
		private static final long serialVersionUID = 1L;

		/** {@inheritDoc} */
		public int compare(CounterRequest request1, CounterRequest request2) {
			if (request1.getHits() > request2.getHits()) {
				return 1;
			} else if (request1.getHits() < request2.getHits()) {
				return -1;
			} else {
				return 0;
			}
		}
	}

	/**
	 * Comparateur pour ordonner les erreurs par heures d'exécution.
	 */
	static final class CounterErrorComparator implements Comparator<CounterError>, Serializable {
		private static final long serialVersionUID = 1L;

		/** {@inheritDoc} */
		public int compare(CounterError error1, CounterError error2) {
			return (int) (error1.getTime() - error2.getTime());
		}
	}

	/**
	 * Comparateur pour ordonner les requêtes en cours par durées écoulées.
	 */
	static final class CounterRequestContextComparator implements
			Comparator<CounterRequestContext>, Serializable {
		private static final long serialVersionUID = 1L;
		private final long timeOfSnapshot;

		CounterRequestContextComparator(long timeOfSnapshot) {
			super();
			this.timeOfSnapshot = timeOfSnapshot;
		}

		/** {@inheritDoc} */
		public int compare(CounterRequestContext context1, CounterRequestContext context2) {
			if (context1.getDuration(timeOfSnapshot) > context2.getDuration(timeOfSnapshot)) {
				return 1;
			} else if (context1.getDuration(timeOfSnapshot) < context2.getDuration(timeOfSnapshot)) {
				return -1;
			} else {
				return 0;
			}
		}
	}

	/**
	 * Constructeur d'un compteur.
	 * @param name Nom du compteur (par exemple: sql...)
	 * @param iconName Icône du compteur (par exemple: db.png)
	 */
	Counter(String name, String iconName) {
		// ici, pas de compteur fils
		this(name, name, iconName, null, new ThreadLocal<CounterRequestContext>());
	}

	/**
	 * Constructeur d'un compteur.
	 * @param name Nom du compteur (par exemple: sql...)
	 * @param storageName Nom unique du compteur pour le stockage (par exemple: sql_20080724)
	 * @param iconName Icône du compteur (par exemple: db.png)
	 * @param childCounterName Nom du compteur fils (par exemple: sql)
	 */
	Counter(String name, String storageName, String iconName, String childCounterName) {
		// ici, pas de compteur fils
		this(name, storageName, iconName, childCounterName,
				new ThreadLocal<CounterRequestContext>());
	}

	/**
	 * Constructeur d'un compteur.
	 * @param name Nom du compteur (par exemple: http...)
	 * @param iconName Icône du compteur (par exemple: db.png)
	 * @param childCounter Compteur fils (par exemple: sqlCounter)
	 */
	Counter(String name, String iconName, Counter childCounter) {
		this(name, name, iconName, childCounter.getName(), childCounter.contextThreadLocal);
	}

	private Counter(String name, String storageName, String iconName, String childCounterName,
			ThreadLocal<CounterRequestContext> contextThreadLocal) {
		super();
		assert name != null;
		assert storageName != null;
		this.storageName = storageName;
		this.name = name;
		this.errorCounter = ERROR_COUNTER_NAME.equals(name) || LOG_COUNTER_NAME.equals(name);
		this.iconName = iconName;
		this.childCounterName = childCounterName;
		this.contextThreadLocal = contextThreadLocal;
		if (errorCounter) {
			this.errors = new LinkedList<CounterError>();
		} else {
			this.errors = null;
		}
	}

	/**
	 * Définit le code de l'application de ce counter (non null).
	 * @param application String
	 */
	void setApplication(String application) {
		assert application != null;
		this.application = application;
	}

	/**
	 * Retourne le code de l'application.
	 * @return String
	 */
	String getApplication() {
		return application;
	}

	/**
	 * Retourne le nom de ce counter (non null).
	 * @return String
	 */
	String getName() {
		return name;
	}

	/**
	 * Retourne le nom de ce counter quand il est stocké sur disque (non null).
	 * @return String
	 */
	String getStorageName() {
		return storageName;
	}

	/**
	 * Retourne le nom de l'icône de ce counter (peut être null).
	 * @return String
	 */
	String getIconName() {
		return iconName;
	}

	/**
	 * Retourne le nom de l'éventuel counter fils (peut être null).
	 * @return String
	 */
	String getChildCounterName() {
		return childCounterName;
	}

	/**
	 * Retourne la date et l'heure de début (non null).
	 * @return Date
	 */
	Date getStartDate() {
		return startDate;
	}

	/**
	 * Définit la date et l'heure de début (non null).
	 * @param startDate Date
	 */
	void setStartDate(Date startDate) {
		assert startDate != null;
		this.startDate = startDate;
	}

	/**
	 * Retourne true si ce counter est affiché dans les rapports.
	 * @return boolean
	 */
	boolean isDisplayed() {
		return displayed;
	}

	/**
	 * Définit si ce counter est affiché dans les rapports.
	 * @param displayed boolean
	 */
	void setDisplayed(boolean displayed) {
		this.displayed = displayed;
	}

	/**
	 * Retourne l'expression régulière permettant de transformer les requêtes de ce counter
	 * avant aggrégation dans les statistiques (peut être null).
	 * @return Pattern
	 */
	Pattern getRequestTransformPattern() {
		return requestTransformPattern;
	}

	/**
	 * Définit l'expression régulière permettant de transformer les requêtes de ce counter
	 * avant aggrégation dans les statistiques.
	 * @param requestTransformPattern Pattern
	 */
	void setRequestTransformPattern(Pattern requestTransformPattern) {
		this.requestTransformPattern = requestTransformPattern;
	}

	/**
	 * Retourne le nombre maximum de requêtes dans ce counter (entier positif).
	 * @return int
	 */
	int getMaxRequestsCount() {
		return maxRequestsCount;
	}

	/**
	 * Définit le nombre maximum de requêtes dans ce counter (entier positif).
	 * @param maxRequestsCount int
	 */
	void setMaxRequestsCount(int maxRequestsCount) {
		assert maxRequestsCount > 0;
		this.maxRequestsCount = maxRequestsCount;
	}

	void bindContext(String requestName, String completeRequestName) {
		bindContext(requestName, completeRequestName, null, -1);
	}

	void bindContext(String requestName, String completeRequestName, String remoteUser,
			long startCpuTime) {
		// requestName est la même chose que ce qui sera utilisée dans addRequest,
		// completeRequestName est la même chose éventuellement complétée
		// pour cette requête à destination de l'affichage dans les requêtes courantes
		// (sinon mettre 2 fois la même chose)
		final CounterRequestContext context = new CounterRequestContext(this, contextThreadLocal
				.get(), requestName, completeRequestName, remoteUser, startCpuTime);
		contextThreadLocal.set(context);
		if (context.getParentContext() == null) {
			rootCurrentContextsByThreadId.put(context.getThreadId(), context);
		}
	}

	void unbindContext() {
		try {
			contextThreadLocal.remove();
		} finally {
			rootCurrentContextsByThreadId.remove(Thread.currentThread().getId());
		}
	}

	void addRequest(String requestName, long duration, long cpuTime, boolean systemError,
			int responseSize) {
		// la méthode addRequest n'est pas synchronisée pour ne pas avoir
		// de synchronisation globale à l'application sur cette instance d'objet
		// ce qui pourrait faire une contention et des ralentissements,
		// par contre la map requests est synchronisée pour les modifications concurrentes

		assert requestName != null;
		assert duration >= 0;
		assert cpuTime >= -1; // -1 pour requêtes sql
		assert responseSize >= -1; // -1 pour requêtes sql

		final String aggregateRequestName = getAggregateRequestName(requestName);

		final CounterRequestContext context = contextThreadLocal.get();
		final CounterRequest request = getCounterRequestInternal(aggregateRequestName);
		synchronized (request) {
			// on synchronise par l'objet request pour éviter de mélanger des ajouts de hits
			// concurrents entre plusieurs threads pour le même type de requête.
			// Rq : on pourrait remplacer ce bloc synchronized par un synchronized
			// sur les méthodes addHit et addChildHits dans la classe CounterRequest.
			request.addHit(duration, cpuTime, systemError, responseSize);

			if (context != null) {
				// on ajoute dans la requête parente toutes les requêtes filles du contexte
				if (context.getParentCounter() == this) {
					request.addChildHits(context);
				}
				request.addChildRequests(context.getChildRequestsExecutionsByRequestId());
			}
		}
		// perf: on fait le reste hors du synchronized sur request
		if (context != null) {
			if (context.getParentCounter() == this) {
				final CounterRequestContext parentContext = context.getParentContext();
				if (parentContext == null) {
					// enlève du threadLocal le contexte que j'ai créé
					// si je suis le counter parent et s'il n'y a pas de contexte parent
					unbindContext();
				} else {
					// on ajoute une requête fille dans le contexte
					context.addChildRequest(this, aggregateRequestName, request.getId(), duration,
							systemError, responseSize);
					// et reporte les requêtes filles dans le contexte parent et rebinde celui-ci
					parentContext.closeChildContext();
					contextThreadLocal.set(parentContext);
				}
			} else {
				// on ajoute une requête fille dans le contexte
				// (à priori il s'agit d'une requête sql)
				context.addChildRequest(this, aggregateRequestName, request.getId(), duration,
						systemError, responseSize);
			}
		}
	}

	void addRequestForSystemError(String requestName, long duration, long cpuTime, String stackTrace) {
		// comme la méthode addRequest, cette méthode n'est pas synchronisée pour ne pas avoir
		// de synchronisation globale à l'application sur cette instance d'objet
		// ce qui pourrait faire une contention et des ralentissements,
		// par contre on synchronise request et errors
		assert requestName != null;
		assert duration >= -1; // -1 pour le counter de log
		assert cpuTime >= -1;
		// on ne doit conserver les stackTraces que pour les compteurs d'erreurs et de logs plus limités en taille
		// car sinon cela risquerait de donner des compteurs trop gros en mémoire et sur disque
		assert errorCounter;
		// le code ci-après suppose qu'il n'y a pas de contexte courant pour les erreurs systèmes
		// contrairement à la méthode addRequest
		assert contextThreadLocal.get() == null;
		final String aggregateRequestName = getAggregateRequestName(requestName);
		final CounterRequest request = getCounterRequestInternal(aggregateRequestName);
		synchronized (request) {
			request.addHit(duration, cpuTime, true, -1);
			if (stackTrace != null) {
				request.setStackTrace(stackTrace);
			}
		}
		synchronized (errors) {
			errors.addLast(new CounterError(requestName, stackTrace));
			if (errors.size() > MAX_ERRORS_COUNT) {
				errors.removeFirst();
			}
		}
	}

	/**
	 * Retourne true si ce counter est un counter d'error
	 * (c'est-à-dire si son nom est "error" ou "log")
	 * @return boolean
	 */
	boolean isErrorCounter() {
		return errorCounter;
	}

	private String getAggregateRequestName(String requestName) {
		final String aggregateRequestName;
		if (requestTransformPattern == null) {
			aggregateRequestName = requestName;
		} else {
			// ce pattern optionnel permet de transformer la description de la requête http
			// pour supprimer des parties variables (identifiant d'objet par exemple)
			// et pour permettre l'agrégation sur cette requête
			aggregateRequestName = requestTransformPattern.matcher(requestName).replaceAll("\\$");
		}
		return aggregateRequestName;
	}

	void addRequestsAndErrors(Counter newCounter) {
		assert getName().equals(newCounter.getName());

		// Pour toutes les requêtes du compteur en paramètre,
		// on ajoute les hits aux requêtes de ce compteur
		// (utilisée dans serveur de collecte).

		// Rq: cette méthode est thread-safe comme les autres méthodes dans cette classe,
		// bien que cela ne soit à priori pas nécessaire telle qu'elle est utilisée dans CollectorServlet
		for (final CounterRequest newRequest : newCounter.getRequests()) {
			if (newRequest.getHits() > 0) {
				final CounterRequest request = getCounterRequestInternal(newRequest.getName());
				synchronized (request) {
					request.addHits(newRequest);
				}
			}
		}

		int size = requests.size();
		final int maxRequests = getMaxRequestsCount();
		if (size > maxRequests) {
			// Si le nombre de requêtes est supérieur à 20000 (sql non bindé par ex.),
			// on essaye ici d'éviter de saturer la mémoire (et le disque dur)
			// avec toutes ces requêtes différentes en éliminant celles ayant moins de 10 hits.
			// (utile pour une agrégation par année dans PeriodCounterFactory par ex.)
			// Mais inutile de le faire dans d'autres méthodes de Counter
			// car ce serait mauvais pour les perfs, cela ne laisserait aucune chance
			// à une nouvelle requête et car cela sera fait par la classe collector
			for (final CounterRequest request : requests.values()) {
				if (request.getHits() < 10) {
					removeRequest(request.getName());
					size--;
					if (size <= maxRequests) {
						break;
					}
				}
			}
		}

		if (isErrorCounter()) {
			addErrors(newCounter.getErrors());
		}
	}

	void addHits(CounterRequest counterRequest) {
		if (counterRequest.getHits() > 0) {
			// clone pour être thread-safe ici
			final CounterRequest newRequest = counterRequest.clone();
			final CounterRequest request = getCounterRequestInternal(newRequest.getName());
			synchronized (request) {
				request.addHits(newRequest);
			}
		}
	}

	void addErrors(List<CounterError> counterErrorList) {
		assert errorCounter;
		if (counterErrorList.isEmpty()) {
			return;
		}
		synchronized (errors) {
			if (!errors.isEmpty() && errors.get(0).getTime() > counterErrorList.get(0).getTime()) {
				// tant que faire se peut on les met à peu près dans l'ordre pour le sort ci après
				errors.addAll(0, counterErrorList);
			} else {
				errors.addAll(counterErrorList);
			}
			if (errors.size() > 1) {
				// "sort" a les mêmes performances sur LinkedList que sur ArrayList car il y a un tableau intermédiaire
				// (selon Implementation Patterns, Kent Beck)
				Collections.sort(errors, new CounterErrorComparator());

				while (errors.size() > MAX_ERRORS_COUNT) {
					errors.removeFirst();
				}
			}
		}
	}

	void removeRequest(String requestName) {
		assert requestName != null;
		requests.remove(requestName);
	}

	/**
	 * Retourne l'objet CounterRequest correspondant au contexte de requête en cours en paramètre.
	 * @param context CounterRequestContext
	 * @return CounterRequest
	 */
	CounterRequest getCounterRequest(CounterRequestContext context) {
		// l'instance de CounterRequest retournée est clonée
		// (nécessaire pour protéger la synchronisation interne du counter),
		// son état peut donc être lu sans synchronisation
		// mais toute modification de cet état ne sera pas conservée
		final String requestName = getAggregateRequestName(context.getRequestName());
		return getCounterRequestInternal(requestName).clone();
	}

	private CounterRequest getCounterRequestInternal(String requestName) {
		CounterRequest request = requests.get(requestName);
		if (request == null) {
			request = new CounterRequest(requestName, getName());
			// putIfAbsent a l'avantage d'être garanti atomique, même si ce n'est pas indispensable
			final CounterRequest precedentRequest = requests.putIfAbsent(requestName, request);
			if (precedentRequest != null) {
				request = precedentRequest;
			}
		}
		return request;
	}

	/**
	 * Retourne le nombre de requêtes dans ce counter.
	 * @return int
	 */
	int getRequestsCount() {
		return requests.size();
	}

	/**
	 * @return Liste des requêtes non triées,
	 * 	la liste et ses objets peuvent être utilisés sans synchronized et sans crainte d'accès concurrents.
	 */
	List<CounterRequest> getRequests() {
		// thread-safe :
		// on crée une copie de la collection et on clone ici chaque CounterRequest de manière synchronisée
		// de manière à ce que l'appelant n'est pas à se préoccuper des synchronisations nécessaires
		// Rq : l'Iterator sur ConcurrentHashMap.values() est garanti ne pas lancer ConcurrentModificationException
		// même s'il y a des ajouts concurrents
		final List<CounterRequest> result = new ArrayList<CounterRequest>(requests.size());
		for (final CounterRequest request : requests.values()) {
			// on synchronize sur request en cas d'ajout en parallèle d'un hit sur cette request
			synchronized (request) {
				result.add(request.clone());
			}
		}
		return result;
	}

	/**
	 * @return Liste des requêtes triées par durée cumulée décroissante,
	 * 	la liste et ses objets peuvent être utilisés sans synchronized et sans crainte d'accès concurrents.
	 */
	List<CounterRequest> getOrderedRequests() {
		final List<CounterRequest> requestList = getRequests();
		if (requestList.size() > 1) {
			Collections.sort(requestList, Collections.reverseOrder(new CounterRequestComparator()));
		}
		return requestList;
	}

	/**
	 * @return Liste des requêtes triées par hits décroissants,
	 * 	la liste et ses objets peuvent être utilisés sans synchronized et sans crainte d'accès concurrents.
	 */
	List<CounterRequest> getOrderedByHitsRequests() {
		final List<CounterRequest> requestList = getRequests();
		if (requestList.size() > 1) {
			Collections.sort(requestList, Collections
					.reverseOrder(new CounterRequestByHitsComparator()));
		}
		return requestList;
	}

	/**
	 * @return Liste des contextes de requêtes courantes triées par durée écoulée décroissante,
	 * 	la liste peut être utilisée sans synchronized et sans crainte d'accès concurrents,
	 *  toutefois les contextes ne sont pas actuellement clonés dans cette méthode.
	 */
	List<CounterRequestContext> getOrderedRootCurrentContexts() {
		final List<CounterRequestContext> contextList = new ArrayList<CounterRequestContext>(
				rootCurrentContextsByThreadId.size());
		for (final CounterRequestContext rootCurrentContext : rootCurrentContextsByThreadId
				.values()) {
			contextList.add(rootCurrentContext.clone());
		}
		if (contextList.size() > 1) {
			Collections.sort(contextList, Collections
					.reverseOrder(new CounterRequestContextComparator(System.currentTimeMillis())));
		}
		return contextList;
	}

	/**
	 * @return Liste des erreurs triée par date croissante,
	 * 	la liste et ses objets peuvent être utilisés sans synchronized et sans crainte d'accès concurrents.
	 */
	List<CounterError> getErrors() {
		if (errors == null) {
			return Collections.emptyList();
		}
		synchronized (errors) {
			return new ArrayList<CounterError>(errors);
		}
	}

	/**
	 * Retourne le nombre d'erreurs dans ce counter.
	 * @return int
	 */
	int getErrorsCount() {
		if (errors == null) {
			return 0;
		}
		synchronized (errors) {
			return errors.size();
		}
	}

	/**
	 * Purge les requêtes, requêtes en cours et erreurs puis positionne la date et heure de début
	 * à l'heure courante.
	 */
	void clear() {
		requests.clear();
		rootCurrentContextsByThreadId.clear();
		if (errors != null) {
			synchronized (errors) {
				errors.clear();
			}
		}
		startDate = new Date();
	}

	/** {@inheritDoc} */
	@Override
	//CHECKSTYLE:OFF
	public Counter clone() { // NOPMD
		//CHECKSTYLE:ON
		final Counter clone = new Counter(getName(), getStorageName(), getIconName(),
				getChildCounterName(), new ThreadLocal<CounterRequestContext>());
		clone.application = getApplication();
		clone.startDate = getStartDate();
		clone.maxRequestsCount = getMaxRequestsCount();
		clone.displayed = isDisplayed();
		clone.requestTransformPattern = getRequestTransformPattern();
		// on ne copie pas rootCurrentContextsByThreadId car on ne fournit pas les requêtes en cours
		// qui sont très rapidement obsolètes au serveur de collecte (et sinon cela poserait la question
		// des clones de parentCounter, de l'agrégation, de la synchro d'horloge pour la durée
		// et des threadId pour la stack-trace),
		// et on ne copie pas contextThreadLocal,
		// et la méthode getRequests() clone les instances de CounterRequest
		for (final CounterRequest request : getRequests()) {
			clone.requests.put(request.getName(), request);
		}
		if (errors != null) {
			clone.errors.addAll(getErrors());
		}
		return clone;
	}

	/**
	 * Enregustre le counter.
	 * @throws IOException e
	 */
	void writeToFile() throws IOException {
		// on clone le counter avant de le sérialiser pour ne pas avoir de problèmes de concurrences d'accès
		final Counter counter = this.clone();
		// on n'écrit pas rootCurrentContextsByThreadId en fichier
		// puisque ces données ne seront plus vrais dans quelques secondes (clear pour être sûr ici)
		counter.rootCurrentContextsByThreadId.clear();
		new CounterStorage(counter).writeToFile();
	}

	/**
	 * Lecture du counter depuis son fichier.
	 * @throws IOException e
	 */
	void readFromFile() throws IOException {
		final Counter counter = new CounterStorage(this).readFromFile();
		if (counter != null) {
			final Counter newCounter = clone();
			startDate = counter.getStartDate();
			requests.clear();
			for (final CounterRequest request : counter.getRequests()) {
				requests.put(request.getName(), request);
			}
			if (errors != null) {
				errors.clear();
				final List<CounterError> counterErrors = counter.getErrors();
				if (counterErrors != null) { // pour compatibilité ascendante
					errors.addAll(counterErrors);
				}
			}
			// on ajoute les nouvelles requêtes enregistrées avant de lire le fichier
			// (par ex. les premières requêtes collectées par le serveur de collecte lors de l'initialisation)
			addRequestsAndErrors(newCounter);
		}
	}

	void migrate() {
		for (final CounterRequest request : requests.values()) {
			request.migrate();
		}
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return getClass().getSimpleName() + "[application=" + getApplication() + ", name="
				+ getName() + ", storageName=" + getStorageName() + ", startDate=" + getStartDate()
				+ ", childCounterName=" + getChildCounterName() + ", " + requests.size()
				+ " requests, " + (errors == null ? "" : errors.size() + " errors, ")
				+ "maxRequestsCount=" + getMaxRequestsCount() + ", displayed=" + isDisplayed()
				+ ']';
	}
}
