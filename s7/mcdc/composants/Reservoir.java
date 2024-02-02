package composants;

public class Reservoir {
	private double capacite = 42; // en litres
	private double volumeDisponible; // en litres

	public R�servoir(double volume) {
		volumeDisponible = volume;
	}

	public double getCapacite() {
		// retourne le volume maximum du r�servoir
		return capacite;
	}

	public double getVolumeDisponible() {
		// retourne le volume courant de carburant disponible
		return volumeDisponible;
	}

	public void consommer(double volumeConsomme) {
		// pour simuler la consommation d'un certain volume de carburant
		volumeDisponible -= volumeConsomme;
	}
}
