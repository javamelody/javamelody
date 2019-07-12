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
package net.bull.javamelody.internal.web.pdf;

import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.io.IOException;

import javax.imageio.ImageIO;

import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.JavaInformations;

/**
 * Construction d'une barre de pourcentage.
 * @author Emeric Vernat
 */
public final class Bar {
	// constantes pour l'affichage d'une barre avec pourcentage
	private static final double MIN_VALUE = 0;
	private static final double MAX_VALUE = 100;
	private static final int PARTIAL_BLOCKS = 5;
	private static final int FULL_BLOCKS = 10;
	private static final double UNIT_SIZE = (MAX_VALUE - MIN_VALUE)
			/ (FULL_BLOCKS * PARTIAL_BLOCKS);

	private final double percentValue;
	private final BufferedImage image;
	private final Graphics graphics;
	private int x; // initialisé à 0
	private final boolean alertOnHighUsage;

	private Bar(double percentValue, boolean alertOnHighUsage) {
		super();
		this.percentValue = percentValue;
		this.alertOnHighUsage = alertOnHighUsage;
		if (alertOnHighUsage) {
			this.image = new BufferedImage(130, 14, BufferedImage.TYPE_INT_ARGB);
		} else {
			this.image = new BufferedImage(106, 10, BufferedImage.TYPE_INT_ARGB);
		}
		this.graphics = image.getGraphics();
	}

	public static BufferedImage toBar(double percentValue) throws IOException {
		final Bar bar = new Bar(percentValue, false);
		bar.draw();
		bar.graphics.dispose();
		return bar.image;
	}

	public static BufferedImage toBarWithAlert(double percentValue) throws IOException {
		final Bar bar = new Bar(percentValue,
				percentValue >= JavaInformations.HIGH_USAGE_THRESHOLD_IN_PERCENTS);
		bar.draw();
		bar.graphics.dispose();
		return bar.image;
	}

	// méthode inspirée de VisualScoreTag dans LambdaProbe/JStripe (Licence GPL)
	private void draw() throws IOException { // NOPMD
		assert x == 0;
		final double myPercent = Math.max(Math.min(percentValue, 100d), 0d);
		final int fullBlockCount = (int) Math.floor(myPercent / (UNIT_SIZE * PARTIAL_BLOCKS));
		final int partialBlockIndex = (int) Math
				.floor((myPercent - fullBlockCount * UNIT_SIZE * PARTIAL_BLOCKS) / UNIT_SIZE);

		addImage(getBarImage(fullBlockCount > 0 || partialBlockIndex > 0 ? "a" : "a0"));

		final BufferedImage fullBody = getBarImage(String.valueOf(PARTIAL_BLOCKS));
		for (int i = 0; i < fullBlockCount; i++) {
			addImage(fullBody);
		}

		if (partialBlockIndex > 0) {
			final BufferedImage partialBody = getBarImage(String.valueOf(partialBlockIndex));
			addImage(partialBody);
		}

		final int emptyBlocks = FULL_BLOCKS - fullBlockCount - (partialBlockIndex > 0 ? 1 : 0);
		final BufferedImage emptyBody = getBarImage(String.valueOf(0));
		for (int i = 0; i < emptyBlocks; i++) {
			addImage(emptyBody);
		}

		addImage(getBarImage(fullBlockCount == FULL_BLOCKS ? "b" : "b0"));

		if (alertOnHighUsage) {
			x += 8;
			graphics.drawImage(getImage("alert.png"), x, 0, null);
		}
	}

	private void addImage(BufferedImage img) {
		graphics.drawImage(img, x, alertOnHighUsage ? 4 : 0, null);
		x += img.getWidth();
	}

	private static BufferedImage getBarImage(String key) throws IOException {
		return getImage("bar/rb_" + key + ".gif");
	}

	private static BufferedImage getImage(String fileName) throws IOException {
		// ici, ne pas utiliser Toolkit.createImage et surtout pas ImageIcon (sur un serveur)
		return ImageIO.read(Bar.class.getResource(Parameters.getResourcePath(fileName)));
	}
}
