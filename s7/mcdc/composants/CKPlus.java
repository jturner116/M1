package composants;

public class CKPlus {
	private CompteurKilometrique monCompteurKilometrique;
	private int kilometragePartiel; // en Km

	public CKPlus(CompteurKilometrique monCompteurKilometrique) {
		this.monCompteurKilometrique = monCompteurKilometrique;
		kilometragePartiel = monCompteurKilometrique.getKilometrage();
	}

	public int distanceParcourue() {
		// retourne le kilom�trage parcouru depuis le dernier reset
		return monCompteurKilometrique.getKilometrage() - kilometragePartiel;
	}

	public void reset() {
		// m�morise le kilom�trage courant pour les futurs calculs de moyenne
		kilometragePartiel = monCompteurKilometrique.getKilometrage();
	}
}
