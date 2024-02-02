package composants;

public class CompteurDeVitesse {
	private double vitesseInstantanée; // en Km/h

	public double getVitesseInstantanée() {
		// retourne la vitesse instantanée
		return vitesseInstantanée;
	}

	public void définirVitesseInstantanée(double vitesseInstantanée) {
		// pour simuler la mesure de la vitesse instantanée
		this.vitesseInstantanée = vitesseInstantanée;
	}
}
