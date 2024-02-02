package composants;

public class CompteurKilom�trique {
	private int kilom�trage = 12; // en Km, kilom�trage au moment de la mise en circulation du v�hicule

	public int getKilom�trage() {
		// retourne le kilom�trage parcouru en Km depuis la mise en circulation du
		// v�hicule
		return kilom�trage;
	}

	public void parcourir(int distance) {// pour simuler un d�placement
		kilom�trage += distance;
	}
}
