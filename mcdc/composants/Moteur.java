package composants;

public class Moteur {
	private double dur�eDeFonctionnement; // en heures

	public double dur�eDeFonctionnement() {
		// retourne la dur�e de fnt du moteur depuis la mise en circulation du v�hicule
		return dur�eDeFonctionnement;
	}

	public void fonctionner(double dur�e) {// pour simuler le fnt du moteur
		dur�eDeFonctionnement += dur�e;
	}
}
