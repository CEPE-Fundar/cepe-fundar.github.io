// ============================================
// nadIA - JavaScript Moderno
// ============================================

// Estado global
const state = {
    scrollPosition: 0,
    isMenuOpen: false
};

// ============================================
// Navbar & Scroll
// ============================================

function initNavbar() {
    const navbar = document.getElementById('navbar');
    const navToggle = document.getElementById('navToggle');
    const navMenu = document.getElementById('navMenu');
    const scrollProgressBar = document.querySelector('.scroll-progress-bar');
    
    // Scroll effects
    window.addEventListener('scroll', () => {
        const scrolled = window.pageYOffset;
        state.scrollPosition = scrolled;
        
        // Add scrolled class
        if (scrolled > 50) {
            navbar.classList.add('scrolled');
        } else {
            navbar.classList.remove('scrolled');
        }
        
        // Update progress bar
        const windowHeight = document.documentElement.scrollHeight - document.documentElement.clientHeight;
        const progress = (scrolled / windowHeight) * 100;
        scrollProgressBar.style.width = `${Math.min(progress, 100)}%`;
    });
    
    // Mobile menu toggle
    if (navToggle && navMenu) {
        navToggle.addEventListener('click', () => {
            state.isMenuOpen = !state.isMenuOpen;
            navToggle.classList.toggle('active');
            navMenu.classList.toggle('active');
        });
        
        // Close menu when clicking outside
        document.addEventListener('click', (e) => {
            if (state.isMenuOpen && !navMenu.contains(e.target) && !navToggle.contains(e.target)) {
                state.isMenuOpen = false;
                navToggle.classList.remove('active');
                navMenu.classList.remove('active');
            }
        });
        
        // Close menu on link click
        navMenu.querySelectorAll('.nav-link').forEach(link => {
            link.addEventListener('click', () => {
                if (state.isMenuOpen) {
                    state.isMenuOpen = false;
                    navToggle.classList.remove('active');
                    navMenu.classList.remove('active');
                }
            });
        });
    }
    
    // Set active nav link based on current page
    const currentPage = window.location.pathname.split('/').pop() || 'index.html';
    document.querySelectorAll('.nav-link').forEach(link => {
        const href = link.getAttribute('href');
        if (href === currentPage || (currentPage === '' && href === 'index.html')) {
            link.classList.add('active');
        }
    });
}

// ============================================
// Back to Top Button
// ============================================

function initBackToTop() {
    const backToTop = document.getElementById('backToTop');
    
    if (!backToTop) return;
    
    window.addEventListener('scroll', () => {
        if (window.pageYOffset > 300) {
            backToTop.classList.add('visible');
        } else {
            backToTop.classList.remove('visible');
        }
    });
    
    backToTop.addEventListener('click', () => {
        window.scrollTo({
            top: 0,
            behavior: 'smooth'
        });
    });
}

// ============================================
// Scroll Animations (AOS-like)
// ============================================

function initScrollAnimations() {
    const observerOptions = {
        threshold: 0.1,
        rootMargin: '0px 0px -50px 0px'
    };
    
    const observer = new IntersectionObserver((entries) => {
        entries.forEach(entry => {
            if (entry.isIntersecting) {
                entry.target.classList.add('aos-animate');
                
                // Unobserve after animation to improve performance
                observer.unobserve(entry.target);
            }
        });
    }, observerOptions);
    
    // Observe all elements with data-aos attribute
    document.querySelectorAll('[data-aos]').forEach(el => {
        observer.observe(el);
    });
}

// ============================================
// Blog Filters
// ============================================

function initBlogFilters() {
    const filterButtons = document.querySelectorAll('.filter-btn');
    const blogEntries = document.querySelectorAll('.blog-entry');
    
    if (!filterButtons.length) return;
    
    // Get category from URL
    const urlParams = new URLSearchParams(window.location.search);
    const categoryFromUrl = urlParams.get('categoria');
    
    // Set active filter from URL
    if (categoryFromUrl) {
        filterButtons.forEach(btn => {
            btn.classList.remove('active');
            if (btn.getAttribute('data-category') === categoryFromUrl) {
                btn.classList.add('active');
                filterEntries(categoryFromUrl);
            }
        });
    }
    
    // Filter button click
    filterButtons.forEach(button => {
        button.addEventListener('click', () => {
            const category = button.getAttribute('data-category');
            
            // Update active state
            filterButtons.forEach(btn => btn.classList.remove('active'));
            button.classList.add('active');
            
            // Update URL
            if (category === 'all') {
                window.history.pushState({}, '', 'blog.html');
            } else {
                window.history.pushState({}, '', `blog.html?categoria=${encodeURIComponent(category)}`);
            }
            
            // Filter entries
            filterEntries(category);
        });
    });
    
    function filterEntries(category) {
        blogEntries.forEach(entry => {
            if (category === 'all') {
                entry.style.display = '';
                entry.style.animation = 'fadeInUp 0.6s ease-out';
            } else {
                const categories = entry.getAttribute('data-categories');
                if (categories && categories.includes(category)) {
                    entry.style.display = '';
                    entry.style.animation = 'fadeInUp 0.6s ease-out';
                } else {
                    entry.style.display = 'none';
                }
            }
        });
    }
}

// ============================================
// Smooth Scroll for Anchor Links
// ============================================

function initSmoothScroll() {
    document.querySelectorAll('a[href^="#"]').forEach(anchor => {
        anchor.addEventListener('click', function (e) {
            const href = this.getAttribute('href');
            if (href === '#') return;
            
            e.preventDefault();
            const target = document.querySelector(href);
            
            if (target) {
                const offsetTop = target.offsetTop - 100;
                window.scrollTo({
                    top: offsetTop,
                    behavior: 'smooth'
                });
            }
        });
    });
}

// ============================================
// Performance: Lazy Load Images
// ============================================

function initLazyLoading() {
    if ('loading' in HTMLImageElement.prototype) {
        // Browser supports lazy loading
        document.querySelectorAll('img[data-src]').forEach(img => {
            img.src = img.dataset.src;
        });
    } else {
        // Fallback for browsers that don't support lazy loading
        const imageObserver = new IntersectionObserver((entries) => {
            entries.forEach(entry => {
                if (entry.isIntersecting) {
                    const img = entry.target;
                    img.src = img.dataset.src;
                    img.classList.add('loaded');
                    imageObserver.unobserve(img);
                }
            });
        });
        
        document.querySelectorAll('img[data-src]').forEach(img => {
            imageObserver.observe(img);
        });
    }
}

// ============================================
// Utilities
// ============================================

// Debounce function
function debounce(func, wait) {
    let timeout;
    return function executedFunction(...args) {
        const later = () => {
            clearTimeout(timeout);
            func(...args);
        };
        clearTimeout(timeout);
        timeout = setTimeout(later, wait);
    };
}

// Throttle function
function throttle(func, limit) {
    let inThrottle;
    return function(...args) {
        if (!inThrottle) {
            func.apply(this, args);
            inThrottle = true;
            setTimeout(() => inThrottle = false, limit);
        }
    };
}

// ============================================
// Initialize Everything
// ============================================

function init() {
    initNavbar();
    initBackToTop();
    initScrollAnimations();
    initBlogFilters();
    initSmoothScroll();
    initLazyLoading();
    
    // Add loaded class to body
    document.body.classList.add('loaded');
    
    console.log('✨ nadIA initialized');
}

// Run on DOM ready
if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
} else {
    init();
}

// Export for use in other modules if needed
if (typeof module !== 'undefined' && module.exports) {
    module.exports = { state, debounce, throttle };
}
