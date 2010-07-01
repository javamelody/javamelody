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

import java.io.Serializable;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Données statistiques d'une requête identifiée, hors paramètres dynamiques comme un identifiant,
 * et sur la période considérée selon le pilotage du collector par l'intermédiaire counter.
 *
 * Les méthodes d'une instance de cette classe ne sont pas thread-safe.
 * L'état d'une instance doit être accédé ou modifié par l'intermédiaire d'une instance de Counter,
 * qui gérera les accès concurrents sur les instances de cette classe.
 * @author Emeric Vernat
 */
class CounterRequest implements Cloneable, Serializable {
	private static final long serialVersionUID = -4301825473892026959L;
	private final String name;
	private final String id;
	// tous ces champs de type long sont initialisés à 0,
	// il peut être supposé que le type long est suffisant
	// sans dépassement de capacité (max : 2^63-1 soit un peu moins de 10^19)
	// et le type long est préféré au type BigInteger pour raison de performances
	private long hits;
	private long durationsSum;
	private long durationsSquareSum;
	private long maximum;
	private long cpuTimeSum;
	private long systemErrors;
	private long responseSizesSum;
	private long childHits;
	private long childDurationsSum;
	private String stackTrace;
	@SuppressWarnings("all")
	private Map<String, Long> childRequestsExecutionsByRequestId;

	/**
	 * Interface du contexte d'une requête en cours.
	 */
	interface ICounterRequestContext {
		/**
		 * @return Nombre de hits du compteur fils pour ce contexte.
		 */
		int getChildHits();

		/**
		 * @return Temps total du compteur fils pour ce contexte.
		 */
		int getChildDurationsSum();

		/**
		 * @return Nombres d'exécutions par requêtes filles
		 */
		Map<String, Long> getChildRequestsExecutionsByRequestId();
	}

	/**
	 * Constructeur.
	 * @param name Nom de la requête
	 * @param counterName Nom du counter
	 */
	CounterRequest(String name, String counterName) {
		super();
		assert name != null;
		assert counterName != null;
		this.name = name;
		this.id = buildId(name, counterName);
	}

	/**
	 * @return Nom de la requête
	 */
	String getName() {
		return name;
	}

	/**
	 * @return Identifiant de la requête, construit à partir de son nom et du nom du counter
	 */
	String getId() {
		return id;
	}

	/**
	 * @return Nombre d'exécution de cette requête
	 */
	long getHits() {
		return hits;
	}

	/**
	 * @return Somme des temps d'exécution de cette requête
	 */
	long getDurationsSum() {
		return durationsSum;
	}

	/**
	 * @return Moyenne des temps d'exécution
	 */
	int getMean() {
		if (hits > 0) {
			return (int) (durationsSum / hits);
		}
		return -1;
	}

	/**
	 * @return écart type (ou sigma, dit "standard deviation" en anglais)
	 */
	int getStandardDeviation() {
		//    soit un ensemble de valeurs Xi
		//    la moyenne est m = somme(Xi) / n,
		//    la déviation de chaque valeur par rapport à la moyenne est di = Xi - m
		//    la variance des valeurs est V = somme(di^2)/(n-1) = somme( (Xi-m)^2 ) / (n-1)
		//			dont on peut dériver V = (somme (Xi^2) - (somme(Xi)^2) / n) / (n-1)
		//			(dont une approximation est V = somme (Xi^2) / n - m^2 mais seulement quand n est élevé).
		//			Cela ne nécessite que de retenir la somme des Xi et la somme des Xi^2
		//			car on ne souhaite pas conserver toutes les valeurs des Xi pour ne pas saturer la mémoire,
		//    et l'écart type (ou sigma) est la racine carrée de la variance : s = sqrt(V)
		//
		//    on dit alors la moyenne est m +/- s

		// Références :
		//      http://web.archive.org/web/20070710000323/http://www.med.umkc.edu/tlwbiostats/variability.html
		//      http://web.archive.org/web/20050512031826/http://helios.bto.ed.ac.uk/bto/statistics/tress3.html
		//		http://www.bmj.com/collections/statsbk/2.html
		if (hits > 0) {
			return (int) Math.sqrt((durationsSquareSum - (double) durationsSum * durationsSum
					/ hits)
					/ (hits - 1));
		}
		return -1;
	}

	/**
	 * @return Maximum des temps d'exécution de cette requête
	 */
	long getMaximum() {
		return maximum;
	}

	/**
	 * @return Somme temps cpu pour l'exécution de cette requête
	 */
	long getCpuTimeSum() {
		return cpuTimeSum;
	}

	/**
	 * @return Moyenne des temps cpu pour l'exécution de cette requête
	 */
	int getCpuTimeMean() {
		if (hits > 0) {
			return (int) (cpuTimeSum / hits);
		}
		return -1;
	}

	/**
	 * @return Pourcentage des erreurs systèmes dans l'exécution de cette requête
	 */
	float getSystemErrorPercentage() {
		// pourcentage d'erreurs systèmes entre 0 et 100,
		// le type de retour est float pour être mesurable
		// car il est probable que le pourcentage soit inférieur à 1%
		if (hits > 0) {
			return Math.min(100f * systemErrors / hits, 100f);
		}
		return 0;
	}

	/**
	 * @return Moyenne des tailles des réponses (http en particulier)
	 */
	int getResponseSizeMean() {
		if (hits > 0) {
			return (int) (responseSizesSum / hits);
		}
		return -1;
	}

	/**
	 * @return Booléen selon qu'il existe des requêtes filles (sql en particulier)
	 */
	boolean hasChildHits() {
		return childHits > 0;
	}

	/**
	 * @return Nombre moyen d'exécutions des requêtes filles (sql en particulier)
	 */
	int getChildHitsMean() {
		if (hits > 0) {
			return (int) (childHits / hits);
		}
		return -1;
	}

	/**
	 * @return Moyenne des temps d'exécutions des requêtes filles (sql en particulier)
	 */
	int getChildDurationsMean() {
		if (hits > 0) {
			return (int) (childDurationsSum / hits);
		}
		return -1;
	}

	/**
	 * @return Map des nombres d'exécutions par requêtes filles
	 */
	Map<String, Long> getChildRequestsExecutionsByRequestId() {
		if (childRequestsExecutionsByRequestId == null) {
			return Collections.emptyMap();
		}
		synchronized (this) {
			return new LinkedHashMap<String, Long>(childRequestsExecutionsByRequestId);
		}
	}

	boolean containsChildRequest(String requestId) {
		if (childRequestsExecutionsByRequestId == null) {
			return false;
		}
		synchronized (this) {
			return childRequestsExecutionsByRequestId.containsKey(requestId);
		}
	}

	/**
	 * @return Dernière stack trace
	 */
	String getStackTrace() {
		return stackTrace;
	}

	void addHit(long duration, long cpuTime, boolean systemError, String systemErrorStackTrace,
			int responseSize) {
		hits++;
		durationsSum += duration;
		durationsSquareSum += duration * duration;
		if (duration > maximum) {
			maximum = duration;
		}
		cpuTimeSum += cpuTime;
		if (systemError) {
			systemErrors++;
		}
		if (systemErrorStackTrace != null) {
			stackTrace = systemErrorStackTrace;
		}
		responseSizesSum += responseSize;
	}

	void addChildHits(ICounterRequestContext context) {
		childHits += context.getChildHits();
		childDurationsSum += context.getChildDurationsSum();
	}

	void addChildRequests(Map<String, Long> childRequests) {
		if (childRequests != null && !childRequests.isEmpty()) {
			if (childRequestsExecutionsByRequestId == null) {
				childRequestsExecutionsByRequestId = new LinkedHashMap<String, Long>(childRequests);
			} else {
				for (final Map.Entry<String, Long> entry : childRequests.entrySet()) {
					final String requestId = entry.getKey();
					Long nbExecutions = childRequestsExecutionsByRequestId.get(requestId);
					if (nbExecutions == null) {
						nbExecutions = entry.getValue();
					} else {
						nbExecutions += entry.getValue();
					}
					childRequestsExecutionsByRequestId.put(requestId, nbExecutions);
				}
			}
		}
	}

	void addHits(CounterRequest request) {
		assert request != null;
		if (request.hits != 0) {
			hits += request.hits;
			durationsSum += request.durationsSum;
			durationsSquareSum += request.durationsSquareSum;
			if (request.maximum > maximum) {
				maximum = request.maximum;
			}
			cpuTimeSum += request.cpuTimeSum;
			systemErrors += request.systemErrors;
			responseSizesSum += request.responseSizesSum;
			childHits += request.childHits;
			childDurationsSum += request.childDurationsSum;
			if (request.stackTrace != null) {
				stackTrace = request.stackTrace;
			}
			addChildRequests(request.childRequestsExecutionsByRequestId);
		}
	}

	void removeHits(CounterRequest request) {
		assert request != null;
		if (request.hits != 0) {
			hits -= request.hits;
			durationsSum -= request.durationsSum;
			durationsSquareSum -= request.durationsSquareSum;
			// on ne peut pas enlever le maximum puisqu'on ne connaît pas le précédent maximum
			//			if (request.maximum > maximum) {
			//				maximum = request.maximum;
			//			}
			cpuTimeSum -= request.cpuTimeSum;
			systemErrors -= request.systemErrors;
			responseSizesSum -= request.responseSizesSum;
			childHits -= request.childHits;
			childDurationsSum -= request.childDurationsSum;

			if (request.childRequestsExecutionsByRequestId != null
					&& childRequestsExecutionsByRequestId != null) {
				for (final Map.Entry<String, Long> entry : request.childRequestsExecutionsByRequestId
						.entrySet()) {
					final String requestId = entry.getKey();
					Long nbExecutions = childRequestsExecutionsByRequestId.get(requestId);
					if (nbExecutions != null) {
						nbExecutions = Math.max(nbExecutions - entry.getValue(), 0);
						if (nbExecutions == 0) {
							childRequestsExecutionsByRequestId.remove(requestId);
							if (childRequestsExecutionsByRequestId.isEmpty()) {
								childRequestsExecutionsByRequestId = null;
								break;
							}
						} else {
							childRequestsExecutionsByRequestId.put(requestId, nbExecutions);
						}
					}
				}
			}
		}
	}

	/** {@inheritDoc} */
	@Override
	public CounterRequest clone() { // NOPMD
		try {
			final CounterRequest clone = (CounterRequest) super.clone();
			if (childRequestsExecutionsByRequestId != null) {
				clone.childRequestsExecutionsByRequestId = new LinkedHashMap<String, Long>(
						getChildRequestsExecutionsByRequestId());
			}
			return clone;
		} catch (final CloneNotSupportedException e) {
			// ne peut arriver puisque CounterRequest implémente Cloneable
			throw new IllegalStateException(e);
		}
	}

	// retourne l'id supposé unique de la requête pour le stockage
	private static String buildId(String name, String counterName) {
		final MessageDigest messageDigest = getMessageDigestInstance();
		messageDigest.update(name.getBytes());
		final byte[] digest = messageDigest.digest();

		final StringBuilder sb = new StringBuilder(digest.length * 2);
		sb.append(counterName);
		// encodage en chaîne hexadécimale,
		// puisque les caractères bizarres ne peuvent être utilisés sur un système de fichiers
		int j;
		for (final byte element : digest) {
			j = element < 0 ? 256 + element : element;
			if (j < 16) {
				sb.append('0');
			}
			sb.append(Integer.toHexString(j));
		}

		return sb.toString();
	}

	private static MessageDigest getMessageDigestInstance() {
		// SHA1 est un algorithme de hashage qui évite les conflits à 2^80 près entre
		// les identifiants supposés uniques (SHA1 est mieux que MD5 qui est mieux que CRC32).
		try {
			return MessageDigest.getInstance("SHA-1");
		} catch (final NoSuchAlgorithmException e) {
			// ne peut arriver car SHA1 est un algorithme disponible par défaut dans le JDK Sun
			throw new IllegalStateException(e);
		}
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return getClass().getSimpleName() + "[name=" + getName() + ", hits=" + getHits() + ", id="
				+ getId() + ']';
	}
}
