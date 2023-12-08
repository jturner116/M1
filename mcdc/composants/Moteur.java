package composants;

public class Moteur {
	private double duréeDeFonctionnement; // en heures

	public double duréeDeFonctionnement() {
		// retourne la durée de fnt du moteur depuis la mise en circulation du véhicule
		return duréeDeFonctionnement;
	}

	public void fonctionner(double durée) {// pour simuler le fnt du moteur
		duréeDeFonctionnement += durée;
	}
}
