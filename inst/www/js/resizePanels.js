// From https://codepen.io/jkasun/pen/QrLjXP

document.addEventListener("DOMContentLoaded", function() {
  
    // Iterate over each container element to apply individual resize behavior
    document.querySelectorAll('.resizable-container').forEach(function(container) {
        var isResizing = false; // Flag to track if resizing is happening
        var lastDownX = 0;      // Variable to store the last X position of the mouse
        
        var leftPanel = container.querySelector('.left-panel');   // Find the left panel within the container
        var rightPanel = container.querySelector('.right-panel'); // Find the right panel within the container
        var resizer = container.querySelector('.resizer');        // Find the resizer (divider) within the container

        // Attach an event listener to handle the mouse down event on the resizer
        resizer.addEventListener('mousedown', function(e) {
            isResizing = true;     // Set the flag to indicate resizing has started
            lastDownX = e.clientX; // Store the current mouse X position

            document.addEventListener('mousemove', resizePanels); // Attach the mousemove event handler
            document.addEventListener('mouseup', stopResizing);   // Attach the mouseup event handler to stop resizing
        });

        // Function to handle the resizing logic when the mouse is moved
        function resizePanels(e) {
            if (!isResizing) return; // If not resizing, exit the function

            // Get the current width of the left and right panels
            var offsetLeftPanel = leftPanel.offsetWidth;
            var offsetRightPanel = rightPanel.offsetWidth;
            var deltaX = e.clientX - lastDownX; // Calculate how much the mouse has moved

            // Update the width (flex-basis) of the left and right panels based on the mouse movement
            leftPanel.style.flexBasis = (offsetLeftPanel + deltaX) + 'px';
            rightPanel.style.flexBasis = (offsetRightPanel - deltaX) + 'px';

            lastDownX = e.clientX; // Update lastDownX to the current mouse position for the next calculation
        }

        // Function to stop resizing when the mouse button is released
        function stopResizing(e) {
            isResizing = false; // Reset the resizing flag
            document.removeEventListener('mousemove', resizePanels); // Remove the mousemove event handler
            document.removeEventListener('mouseup', stopResizing);   // Remove the mouseup event handler
        }
    });
});