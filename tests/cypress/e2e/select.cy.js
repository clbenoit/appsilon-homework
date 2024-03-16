describe('should have more than one available choice', () => {
  beforeEach(() => {
    cy.visit('/');
  });
  it('checks the availability of options', () => {
    cy.get('select#app-select_species-taxonRank option:selected')
        .should('have.value', '1')
        
    cy.get('select#app-select_species-kingdom option:selected')
       .should('have.value', "Animalia")
  });
});