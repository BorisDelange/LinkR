function initSortableTabs(containerId) {
  const tabsContainer = document.getElementById(containerId);
  if (!tabsContainer) return;
  
  const config = {
    idPrefix: tabsContainer.dataset.idPrefix || '',
    inputId: tabsContainer.dataset.inputId || containerId,
    draggableClass: tabsContainer.dataset.draggableClass || 'tab'
  };
  
  if (tabsContainer.sortableInstance) {
    tabsContainer.sortableInstance.destroy();
  }
  
  tabsContainer.sortableInstance = new Sortable(tabsContainer, {
    animation: 150,
    draggable: '.' + config.draggableClass,
    handle: '.' + config.draggableClass,
    onEnd: function(evt) {
      const tabs = Array.from(tabsContainer.getElementsByClassName(config.draggableClass.replace('.', '')));
      
      const positions = tabs.map((tab, index) => {
        const fullId = tab.id;
        const tabId = parseInt(fullId.replace(config.idPrefix, ''));
        return {
          id: tabId,
          position: index + 1
        };
      });

      Shiny.setInputValue(
        config.inputId,
        positions,
        {priority: 'event'}
      );
    }
  });
}