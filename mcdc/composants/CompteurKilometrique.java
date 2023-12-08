package composants;

public class CompteurKilométrique {
	private int kilométrage = 12; // en Km, kilométrage au moment de la mise en circulation du véhicule

	public int getKilométrage() {
		// retourne le kilométrage parcouru en Km depuis la mise en circulation du
		// véhicule
		return kilométrage;
	}

	public void parcourir(int distance) {// pour simuler un déplacement
		kilométrage += distance;
	}
}
