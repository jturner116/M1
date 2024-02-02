package composants;

public class Facade {
    private Moteur monMoteur;
    private Reservoir monReservoir;
    private Plus monPlus;
    
    public int getKilometrage() {
        return monPlus.distanceParcourue();
    }

    public int getDistanceParcourue() {
        return monCompteurKilo.distanceParcourue();
    }

    public double getVitesseMoyenne() {
        return monCompteurVitesse.vitesseMoyenne();
    }

    public double getCarburantRestant() {
        return monReservoir.getCapacite() - monReservoir.getVolumeDisponible();
    }

    public double getConsommationMoyenne() {
        return 
    }    
}